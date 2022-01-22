#![allow(dead_code)]

use core::fmt;
use std::{
    cmp,
    collections::HashMap,
    env::{self},
    fs, ops, process,
};

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

    /// A statement can be either an assignment or function call followed by a newline:
    /// statement ::= (<assignment> | <funccall>) "\n"
    /// assignment ::= <identifier> "=" <expression>
    /// funccall ::= <identifier> (<string_literal> | "(" <expression> {"," <expression>} ")")
    fn parse_statement(&mut self) -> ParserResult<Box<dyn ASTNode>> {
        let ident = self.consume();

        if !ident.is_type(TokenType::Identifier) {
            return self.create_error_at_token(
                &ident,
                &format!("Unexpected statement start `{}`", ident),
                "",
            );
        }

        if self.match_and_advance(TokenType::Assignment) {
            return self.parse_assignment(ident);
        }

        let current_type = self.current.as_ref().unwrap().r#type;
        if current_type == TokenType::String || current_type == TokenType::LeftParenthesis {
            return self.parse_function_call(ident);
        }

        return self.create_error_at_token(
            self.current.as_ref().unwrap(),
            &format!(
                "Expected assignment or function call, found `{}`",
                self.current.as_ref().unwrap()
            ),
            "expected here",
        );
    }

    fn parse_assignment(&mut self, ident: Token) -> ParserResult<Box<dyn ASTNode>> {
        let expression = self.parse_expression(Precedence::Assignment as u32)?;
        Ok(Box::new(NodeAssignment {
            identifier: NodeVariant(ident.into()),
            expression,
        }))
    }

    fn parse_function_call(&mut self, ident: Token) -> ParserResult<Box<dyn ASTNode>> {
        if self.current.as_ref().unwrap().is_type(TokenType::String) {
            let arg = self.consume();
            let mut args = NodeExpressionList(vec![]);
            args.push(Box::new(NodeVariant(arg.into())));
            return Ok(Box::new(NodeCall {
                func: ident.lexeme,
                args,
            }));
        }

        self.expect(
            TokenType::LeftParenthesis,
            "Expected `(` at the start of function call",
        )?;

        let mut args = NodeExpressionList(vec![]);
        let expression = self.parse_expression(Precedence::Assignment as u32)?;
        args.push(expression);

        while self.match_and_advance(TokenType::Comma) {
            let expression = self.parse_expression(Precedence::Assignment as u32)?;
            args.push(expression);
        }

        self.expect(
            TokenType::RightParenthesis,
            "Expected `)` at the end of function call",
        )?;

        Ok(Box::new(NodeCall {
            func: ident.lexeme,
            args,
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

        // check for unary operator
        if lhs.is_type(TokenType::Minus) {
            return Ok(Box::new(NodeUnaryOperation {
                op: "-".to_string(),
                rhs: self.parse_expression(Precedence::Unary as u32).unwrap(),
            }));
        }

        if !lhs.is_any_type(&[
            TokenType::Identifier,
            TokenType::Number,
            TokenType::String,
            TokenType::Boolean,
        ]) {
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
                TokenType::GreaterThan,
                TokenType::GreaterOrEqualThan,
                TokenType::LowerThan,
                TokenType::LowerOrEqualThan,
                TokenType::Equality,
                TokenType::Inequality,
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

struct NodeUnaryOperation {
    op: String,
    rhs: Box<dyn ASTNode>,
}

impl fmt::Display for NodeUnaryOperation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("[{}] {}", self.op, self.rhs))
    }
}

impl ASTNode for NodeUnaryOperation {
    fn execute(&self, context: &mut ExecutionContext) {
        self.rhs.execute(context);
        let rhs = context.pop();
        match self.op.as_str() {
            "-" => context.push(-rhs),
            _ => panic!("RuntimeError: invalid unary operand {}", self.op),
        }
    }
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
            ">" => context.push(Variant::Boolean(lhs > rhs)),
            ">=" => context.push(Variant::Boolean(lhs >= rhs)),
            "<" => context.push(Variant::Boolean(lhs < rhs)),
            "<=" => context.push(Variant::Boolean(lhs <= rhs)),
            "==" => context.push(Variant::Boolean(lhs == rhs)),
            "!=" => context.push(Variant::Boolean(lhs != rhs)),
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
                context.push(context.variable_lookup(ident).unwrap().clone())
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

        context.call_native_function(&self.func, args);
    }
}

mod helius_std {
    use crate::Variant;

    pub fn print(args: Vec<Variant>) -> Vec<Variant> {
        for arg in args.iter().rev() {
            print!("{} ", arg);
        }
        println!();
        Vec::new()
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
pub enum Variant {
    Identifier(String),
    String(String),
    Number(i32),
    Boolean(bool),
}

impl fmt::Display for Variant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Variant::Identifier(ident) => f.write_fmt(format_args!("{}", ident)),
            Variant::String(s) => f.write_fmt(format_args!("{}", s)),
            Variant::Number(n) => f.write_fmt(format_args!("{}", n)),
            Variant::Boolean(b) => f.write_str(if *b { "True" } else { "False" }),
        }
    }
}

impl From<Token> for Variant {
    fn from(token: Token) -> Self {
        match token.r#type {
            TokenType::Identifier => Self::Identifier(token.lexeme),
            TokenType::Number => Self::Number(token.lexeme.parse().unwrap()),
            TokenType::String => Self::String(token.lexeme),
            TokenType::Boolean => Self::Boolean(token.lexeme == "true"),
            _ => panic!("Invalid conversion for token {:?}", token),
        }
    }
}

impl cmp::PartialEq for Variant {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            (Self::Number(l0), Self::Number(r0)) => l0 == r0,
            (Self::Boolean(b0), Self::Boolean(b1)) => b0 == b1,
            _ => panic!("RuntimeError: cannot compare {:?} and {:?}", self, other),
        }
    }
}

impl cmp::PartialOrd for Variant {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        match (self, other) {
            (Variant::Number(n1), Variant::Number(n2)) => n1.partial_cmp(n2),
            _ => panic!("RuntimeError: cannot compare {:?} and {:?}", self, other),
        }
    }
}

impl ops::Add for Variant {
    type Output = Variant;

    fn add(self, rhs: Self) -> Self::Output {
        if let (Variant::Number(lhs), Variant::Number(rhs)) = (&self, &rhs) {
            Variant::Number(*lhs + *rhs)
        } else {
            panic!("RuntimeError: cannot add {:?} and {:?}", self, rhs);
        }
    }
}

impl ops::Sub for Variant {
    type Output = Variant;

    fn sub(self, rhs: Self) -> Self::Output {
        if let (Variant::Number(lhs), Variant::Number(rhs)) = (&self, &rhs) {
            Variant::Number(*lhs - *rhs)
        } else {
            panic!("RuntimeError: cannot add {:?} and {:?}", self, rhs);
        }
    }
}

impl ops::Mul for Variant {
    type Output = Variant;

    fn mul(self, rhs: Self) -> Self::Output {
        if let (Variant::Number(lhs), Variant::Number(rhs)) = (&self, &rhs) {
            Variant::Number(*lhs * *rhs)
        } else {
            panic!("RuntimeError: cannot add {:?} and {:?}", self, rhs);
        }
    }
}

impl ops::Div for Variant {
    type Output = Variant;

    fn div(self, rhs: Self) -> Self::Output {
        if let (Variant::Number(lhs), Variant::Number(rhs)) = (&self, &rhs) {
            Variant::Number(*lhs / *rhs)
        } else {
            panic!("RuntimeError: cannot add {:?} and {:?}", self, rhs);
        }
    }
}

impl ops::Neg for Variant {
    type Output = Variant;

    fn neg(self) -> Self::Output {
        if let Variant::Number(n) = self {
            Variant::Number(-n)
        } else {
            panic!(
                "RuntimeError: cannot apply unary operator `-` to {:?}",
                self
            );
        }
    }
}

#[derive(Default)]
struct ExecutionContext {
    variables: HashMap<String, Variant>,
    stack: Vec<Variant>,
    native_functions: HashMap<String, Box<&'static dyn Fn(Vec<Variant>) -> Vec<Variant>>>,
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

    fn add_native_function<F>(&mut self, name: &str, func: &'static F)
    where
        F: Fn(Vec<Variant>) -> Vec<Variant>,
    {
        self.native_functions
            .insert(name.to_owned(), Box::new(func));
    }

    fn call_native_function(&mut self, name: &str, args: Vec<Variant>) {
        let f = match self.native_functions.get(name) {
            Some(f) => f,
            None => panic!("Trying to call non existent function"),
        };
        let results = f(args);
        for result in results.into_iter().rev() {
            self.push(result);
        }
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
            show_usage(program_name, &format!("{}", err));
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
        native_functions: HashMap::new(),
    };

    context.add_native_function("print", &helius_std::print);

    for stmt in program {
        stmt.execute(&mut context);
    }
}
