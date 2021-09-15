#![allow(dead_code)]

use core::fmt;
use std::{collections::HashMap, env, fs, ops, process};

mod tokenizer;
use tokenizer::{Token, TokenType, Tokenizer};

use crate::tokenizer::Precedence;

struct Parser {
    tokenizer: Tokenizer,
    current: Option<Token>,
    has_error: bool,
}

#[derive(Debug)]
struct ParserError {
    msg: String,
    short_msg: String,
    line: usize,
    column: usize,
}

type ParserResult<T = ()> = Result<T, ParserError>;

impl Parser {
    fn new(tokenizer: Tokenizer) -> Self {
        Parser {
            tokenizer,
            current: None,
            has_error: false,
        }
    }

    fn parse(&mut self) -> Vec<Box<dyn ASTNode>> {
        self.program()
    }

    fn expect(&mut self, token_type: TokenType, msg: &str) -> ParserResult {
        if !self.match_and_advance(token_type) {
            return self.create_error_at_token(
                self.current.as_ref().unwrap(),
                &format!(
                    "Expected token {:?} got {:?}\n{}",
                    token_type,
                    self.current.as_ref().unwrap(),
                    msg
                ),
                "expected here",
            );
        }
        Ok(())
    }

    fn program(&mut self) -> Vec<Box<dyn ASTNode>> {
        let mut statements: Vec<Box<dyn ASTNode>> = vec![];

        self.advance();
        loop {
            while self.match_and_advance(TokenType::Newline) {}

            if self.match_and_advance(TokenType::Eof) {
                break;
            }

            match self.parse_statement() {
                Ok(stmt) => statements.push(stmt),
                Err(e) => {
                    self.print_error(e);
                    self.recover_from_error();
                }
            }
        }

        if self.has_error {
            println!("error: can't compile program due to previous errors");
            process::exit(1);
        }

        statements
    }

    fn recover_from_error(&mut self) {
        self.has_error = true;

        while !self
            .current
            .as_ref()
            .unwrap()
            .is_any_type(&[TokenType::Newline, TokenType::Eof])
        {
            self.consume();
        }
    }

    fn match_and_advance(&mut self, token_type: TokenType) -> bool {
        if self.current.as_ref().unwrap().is_type(token_type) {
            self.advance();
            return true;
        }
        false
    }

    fn advance(&mut self) {
        self.current = match self.tokenizer.next() {
            Ok(token) => Some(token),
            Err(err) => panic!("Lexical error {:?}", err),
        }
    }

    fn parse_statement(&mut self) -> ParserResult<Box<dyn ASTNode>> {
        match self.current.as_ref().unwrap().r#type {
            TokenType::Identifier => self.parse_assignment(),
            TokenType::Print => self.parse_print(),
            _ => self.create_error_at_token(
                self.current.as_ref().unwrap(),
                &format!(
                    "Unexpected statement start `{}`",
                    &self.current.as_ref().unwrap()
                ),
                "",
            ),
        }
    }

    fn parse_assignment(&mut self) -> ParserResult<Box<dyn ASTNode>> {
        assert!(self.current.as_ref().unwrap().r#type == TokenType::Identifier);
        let ident = self.consume();
        self.expect(TokenType::Assignment, "Expected after identifier")?;

        let expression = self.parse_expression(Precedence::Assignment as u32)?;
        Ok(Box::new(NodeAssignment {
            identifier: NodeVariant(ident.into()),
            expression,
        }))
    }

    fn parse_print(&mut self) -> ParserResult<Box<dyn ASTNode>> {
        assert!(self.match_and_advance(TokenType::Print));
        let mut node = NodeExpressionList(vec![]);
        let expression = self.parse_expression(Precedence::Assignment as u32)?;
        node.push(expression);

        while self.match_and_advance(TokenType::Comma) {
            let expression = self.parse_expression(Precedence::Assignment as u32)?;
            node.push(expression);
        }
        Ok(Box::new(NodeCall {
            func: "print".to_string(),
            args: node,
        }))
    }

    fn create_error_at_token<T>(
        &self,
        token: &Token,
        msg: &str,
        short_msg: &str,
    ) -> ParserResult<T> {
        Err({
            ParserError {
                msg: msg.to_string(),
                short_msg: short_msg.to_string(),
                line: token.line,
                column: token.column,
            }
        })
    }

    fn parse_expression(&mut self, precedence: u32) -> ParserResult<Box<dyn ASTNode>> {
        // prefix expression
        let lhs = self.consume();
        if !lhs.is_any_type(&[TokenType::Identifier, TokenType::Number, TokenType::String]) {
            return self.create_error_at_token(
                &lhs,
                &format!("Expected expression, found `{}`", &lhs),
                "expression should be here",
            );
        }
        let mut expr_node: Box<dyn ASTNode> = Box::new(NodeVariant(lhs.into()));

        // infix expression
        while precedence < self.current.as_ref().unwrap().get_precedence() {
            let op = self.consume();
            if !op.is_any_type(&[
                TokenType::Plus,
                TokenType::Minus,
                TokenType::Mul,
                TokenType::Div,
            ]) {
                return self.create_error_at_token(
                    &op,
                    &format!("Expected binary operation, found {:?}", op),
                    "",
                );
            }
            let rhs = match self.parse_expression(op.get_precedence()) {
                Ok(expr) => expr,
                Err(err) => {
                    return self.create_error_at_token(
                        &op,
                        &format!("{}. Expected after `{}` token.", err.msg, op.lexeme),
                        "after this",
                    );
                }
            };
            expr_node = Box::new(NodeBinaryOperation {
                lhs: expr_node,
                op: op.lexeme,
                rhs,
            });
        }
        Ok(expr_node)
    }

    fn consume(&mut self) -> Token {
        let t = self.current.to_owned().unwrap();
        self.advance();
        t
    }

    fn print_error(&self, e: ParserError) {
        println!("{}:{}: error: {}", e.line, e.column, e.msg);
        println!("  {:1$} |", "", format!("{}", e.line).len());
        println!("  {} | {}", e.line, self.tokenizer.get_source_line(e.line));
        println!(
            "{}^{} {}\n",
            format!(
                "  {0:1$} | {2:3$}",
                "",
                format!("{}", e.line).len(),
                "",
                e.column - 1
            ),
            if !e.short_msg.is_empty() { "--" } else { "" },
            e.short_msg
        );
    }
}

trait ASTNode: fmt::Display {
    fn execute(&self, context: &mut ExecutionContext);
}

struct NodeBinaryOperation {
    lhs: Box<dyn ASTNode>,
    op: String,
    rhs: Box<dyn ASTNode>,
}

impl fmt::Display for NodeBinaryOperation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("[{}] {} {}", self.op, self.lhs, self.rhs))
    }
}

impl ASTNode for NodeBinaryOperation {
    fn execute(&self, context: &mut ExecutionContext) {
        self.lhs.execute(context);
        self.rhs.execute(context);
        let rhs = context.pop();
        let lhs = context.pop();
        match self.op.as_str() {
            "+" => context.push(lhs + rhs),
            "-" => context.push(lhs - rhs),
            "*" => context.push(lhs * rhs),
            "/" => context.push(lhs / rhs),
            _ => panic!("RuntimeError: invalid operand {}", self.op),
        }
    }
}

struct NodeVariant(Variant);
impl fmt::Display for NodeVariant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{:?}", self.0))
    }
}
impl ASTNode for NodeVariant {
    fn execute(&self, context: &mut ExecutionContext) {
        match &self.0 {
            Variant::Identifier(ident) => {
                context.push(context.variable_lookup(&ident).unwrap().clone())
            }
            _ => context.push(self.0.clone()),
        }
    }
}

struct NodeExpressionList(Vec<Box<dyn ASTNode>>);
impl NodeExpressionList {
    fn push(&mut self, value: Box<dyn ASTNode>) {
        self.0.push(value)
    }
}

impl fmt::Display for NodeExpressionList {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let count = self.0.len();
        for (i, node) in self.0.iter().enumerate() {
            f.write_fmt(format_args!("{}", node))?;
            if i < count - 1 {
                f.write_str(", ")?;
            }
        }
        Ok(())
    }
}
impl ASTNode for NodeExpressionList {
    fn execute(&self, context: &mut ExecutionContext) {
        for expr in &self.0 {
            expr.execute(context);
        }
    }
}

struct NodeCall {
    func: String,
    args: NodeExpressionList,
}
impl fmt::Display for NodeCall {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{}({})", self.func, self.args))
    }
}
impl ASTNode for NodeCall {
    fn execute(&self, context: &mut ExecutionContext) {
        // Assume print for now.
        self.args.execute(context);
        let mut args = vec![];
        for _ in 0..self.args.0.len() {
            args.push(context.pop());
        }
        for arg in args.iter().rev() {
            print!("{} ", arg);
        }
        println!();
    }
}

struct NodeAssignment {
    identifier: NodeVariant,
    expression: Box<dyn ASTNode>,
}

impl fmt::Display for NodeAssignment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{} = {}", self.identifier, self.expression))
    }
}
impl ASTNode for NodeAssignment {
    fn execute(&self, context: &mut ExecutionContext) {
        self.expression.execute(context);
        let value = context.pop();
        if let Variant::Identifier(identifier) = &self.identifier.0 {
            context.variable_set(identifier, value);
        }
    }
}

#[derive(Debug, Clone)]
enum Variant {
    Identifier(String),
    String(String),
    Number(i32),
}

impl fmt::Display for Variant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Variant::Identifier(ident) => f.write_fmt(format_args!("{}", ident)),
            Variant::String(s) => f.write_fmt(format_args!("{}", s)),
            Variant::Number(n) => f.write_fmt(format_args!("{}", n)),
        }
    }
}

impl From<Token> for Variant {
    fn from(token: Token) -> Self {
        match token.r#type {
            TokenType::Identifier => Self::Identifier(token.lexeme),
            TokenType::Number => Self::Number(token.lexeme.parse().unwrap()),
            TokenType::String => Self::String(token.lexeme),
            _ => panic!("Invalid conversion for token {:?}", token),
        }
    }
}

impl ops::Add for Variant {
    type Output = Variant;

    fn add(self, rhs: Self) -> Self::Output {
        if !matches!(&self, Variant::Number(_)) || !matches!(&rhs, &Variant::Number(_)) {
            panic!("RuntimeError: cannot add {:?} and {:?}", self, rhs);
        }

        if let Variant::Number(lhs) = self {
            if let Variant::Number(rhs) = rhs {
                Variant::Number(lhs + rhs)
            } else {
                panic!("unreachable");
            }
        } else {
            panic!("unreachable");
        }
    }
}

impl ops::Sub for Variant {
    type Output = Variant;

    fn sub(self, rhs: Self) -> Self::Output {
        if !matches!(&self, Variant::Number(_)) || !matches!(&rhs, &Variant::Number(_)) {
            panic!("RuntimeError: cannot subtract {:?} and {:?}", self, rhs);
        }

        if let Variant::Number(lhs) = self {
            if let Variant::Number(rhs) = rhs {
                Variant::Number(lhs - rhs)
            } else {
                panic!("unreachable");
            }
        } else {
            panic!("unreachable");
        }
    }
}

impl ops::Mul for Variant {
    type Output = Variant;

    fn mul(self, rhs: Self) -> Self::Output {
        if !matches!(&self, Variant::Number(_)) || !matches!(&rhs, &Variant::Number(_)) {
            panic!("RuntimeError: cannot multiply {:?} and {:?}", self, rhs);
        }

        if let Variant::Number(lhs) = self {
            if let Variant::Number(rhs) = rhs {
                Variant::Number(lhs * rhs)
            } else {
                panic!("unreachable");
            }
        } else {
            panic!("unreachable");
        }
    }
}

impl ops::Div for Variant {
    type Output = Variant;

    fn div(self, rhs: Self) -> Self::Output {
        if !matches!(&self, Variant::Number(_)) || !matches!(&rhs, &Variant::Number(_)) {
            panic!("RuntimeError: cannot divide {:?} and {:?}", self, rhs);
        }

        if let Variant::Number(lhs) = self {
            if let Variant::Number(rhs) = rhs {
                Variant::Number(lhs / rhs)
            } else {
                panic!("unreachable");
            }
        } else {
            panic!("unreachable");
        }
    }
}

#[derive(Default)]
struct ExecutionContext {
    variables: HashMap<String, Variant>,
    stack: Vec<Variant>,
}

impl ExecutionContext {
    fn push(&mut self, value: Variant) {
        self.stack.push(value);
    }

    fn pop(&mut self) -> Variant {
        self.stack.pop().unwrap()
    }

    fn variable_lookup(&self, name: &str) -> Option<&Variant> {
        self.variables.get(name)
    }

    fn variable_set(&mut self, name: &str, value: Variant) {
        self.variables.insert(name.to_string(), value);
    }
}

trait CompilationUnit {
    fn execute(&self, context: &mut ExecutionContext);
}

impl CompilationUnit for Variant {
    fn execute(&self, context: &mut ExecutionContext) {
        context.push(self.clone());
    }
}

fn show_usage(program_name: &str, error_msg: &str) {
    println!("usage: {} file_name", program_name);
    println!("error: {}", error_msg)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let program_name = &args[0];
    if args.len() != 2 {
        show_usage(program_name, "file_name is required");
        process::exit(1);
    }

    let file_name = &args[1];
    let file_contents = match fs::read(file_name) {
        Ok(contents) => contents,
        Err(err) => {
            show_usage(&program_name, &format!("{}", err));
            process::exit(1);
        }
    };

    let program_source = String::from_utf8_lossy(&file_contents).to_string();

    let tokenizer = Tokenizer::new(program_source);
    let mut parser = Parser::new(tokenizer);

    let program = parser.parse();
    let mut context = ExecutionContext {
        variables: HashMap::new(),
        stack: vec![],
    };

    for stmt in program {
        stmt.execute(&mut context);
    }
}
