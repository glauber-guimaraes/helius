#![allow(dead_code)]

use std::{
    collections::HashMap,
    env::{self},
    fmt, fs, process,
    rc::Rc,
};

mod tokenizer;
use tokenizer::{Precedence, Token, TokenType, Tokenizer};

mod variant;
use variant::{NativeFunction, Variant};

struct Parser {
    tokenizer: Tokenizer,
    current: Option<Token>,
    has_error: bool,
    functions: Vec<NodeFunctionBlock>,
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
            functions: vec![],
        }
    }

    fn parse(&mut self) -> Vec<Box<dyn ASTNode>> {
        self.program()
    }

    fn expect(&mut self, token_type: TokenType, msg: &str) -> ParserResult<Token> {
        let current = self.current.as_ref().unwrap().clone();
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
        Ok(current)
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

        if ident.is_any_type(&[TokenType::If, TokenType::While]) {
            let result = match ident.r#type {
                TokenType::If => self.parse_conditional(),
                TokenType::While => self.parse_loop(),
                _ => unreachable!(),
            };

            if result.is_err() {
                while !self
                    .current
                    .as_ref()
                    .unwrap()
                    .is_any_type(&[TokenType::End, TokenType::Eof])
                {
                    self.consume();
                }
            }

            return result;
        }

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

    fn parse_loop(&mut self) -> ParserResult<Box<dyn ASTNode>> {
        let condition = self.parse_expression(Precedence::Assignment as u32)?;
        self.expect(TokenType::Do, "")?;

        let mut block = vec![];

        loop {
            while self.match_and_advance(TokenType::Newline) {}

            if self.match_and_advance(TokenType::Eof) {
                break;
            }

            if self.match_and_advance(TokenType::End) {
                break;
            }

            match self.parse_statement() {
                Ok(stmt) => block.push(stmt),
                Err(e) => {
                    self.print_error(e);
                    self.recover_from_error();
                }
            }
        }

        Ok(Box::new(NodeLoop { condition, block }))
    }

    fn parse_conditional(&mut self) -> ParserResult<Box<dyn ASTNode>> {
        let condition = self.parse_expression(Precedence::Assignment as u32)?;
        self.expect(TokenType::Then, "")?;

        let mut true_block = vec![];
        let mut false_block = vec![];
        let mut parsing_block = &mut true_block;

        let mut has_else_block = false;

        loop {
            while self.match_and_advance(TokenType::Newline) {}

            if self.match_and_advance(TokenType::Eof) {
                break;
            }

            if self.match_and_advance(TokenType::End) {
                break;
            }

            if self.current.as_ref().unwrap().is_type(TokenType::Else) {
                let else_token = self.current.to_owned().unwrap();
                self.advance();

                match (has_else_block, self.current.as_ref().unwrap().r#type) {
                    (true, TokenType::If) => {
                        return self.create_error_at_token(
                            &else_token,
                            "Conditional block cannot have `else if` defined after `else`",
                            "branch started here",
                        )
                    }
                    (true, _) => {
                        return self.create_error_at_token(
                            &else_token,
                            "Conditional block cannot have multiple `else` branches",
                            "branch started here",
                        )
                    }
                    (_, _) => {}
                };

                if self.match_and_advance(TokenType::If) {
                    let elseif = self.parse_conditional()?;
                    false_block.push(elseif);
                    break;
                }
                has_else_block = true;
                parsing_block = &mut false_block;
                continue;
            }

            match self.parse_statement() {
                Ok(stmt) => parsing_block.push(stmt),
                Err(e) => {
                    self.print_error(e);
                    self.recover_from_error();
                }
            }
        }

        Ok(Box::new(NodeConditional {
            condition,
            true_block,
            false_block,
        }))
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

        if !self.match_and_advance(TokenType::RightParenthesis) {
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
        }

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

    fn peek_type(&self) -> TokenType {
        self.current.as_ref().unwrap().r#type.to_owned()
    }

    fn parse_function_definition(&mut self) -> ParserResult<Box<dyn ASTNode>> {
        self.expect(
            TokenType::LeftParenthesis,
            "Expect `(` before argument list",
        )?;

        let mut args = vec![];
        if !self.match_and_advance(TokenType::RightParenthesis) {
            let ident = self.expect(TokenType::Identifier, "")?;
            args.push(ident);

            while self.match_and_advance(TokenType::Comma) {
                let expression = self.expect(TokenType::Identifier, "")?;
                args.push(expression);
            }
            self.expect(
                TokenType::RightParenthesis,
                "Expected `)` at the end of function call",
            )?;
        }
        let args: Vec<String> = args.into_iter().map(|arg| arg.lexeme).collect();

        let mut block = vec![];

        loop {
            while self.match_and_advance(TokenType::Newline) {}

            if self.match_and_advance(TokenType::Eof) {
                return Err(ParserError {
                    msg: "Reached end of file while parsing function definition".to_string(),
                    short_msg: "".to_string(),
                    line: 0,
                    column: 0,
                });
            }

            if self.match_and_advance(TokenType::End) {
                break;
            }

            match self.parse_statement() {
                Ok(stmt) => block.push(stmt),
                Err(e) => {
                    self.print_error(e);
                    self.recover_from_error();
                }
            }
        }

        let function_id = self.functions.len() as u32;
        self.functions.push(NodeFunctionBlock {
            arg_names: args,
            block,
        });

        Ok(Box::new(NodeVariant(Variant::Function(function_id))))
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

        if lhs.is_type(TokenType::Function) {
            return self.parse_function_definition();
        }

        if lhs.is_type(TokenType::LeftParenthesis) {
            let parenthesis_expression = self.parse_expression(Precedence::Assignment as u32);

            self.expect(TokenType::RightParenthesis, "Expected `)` after `(` token")?;

            return parenthesis_expression;
        }

        if !lhs.is_any_type(&[
            TokenType::Identifier,
            TokenType::Number,
            TokenType::String,
            TokenType::Boolean,
            TokenType::None,
        ]) {
            return self.create_error_at_token(
                &lhs,
                &format!("Expected expression, found `{}`", &lhs),
                "expression should be here",
            );
        }

        let mut expr_node: Box<dyn ASTNode>;

        if lhs.is_type(TokenType::Identifier) && self.peek_type() == TokenType::LeftParenthesis {
            expr_node = self.parse_function_call(lhs)?;
        } else {
            expr_node = Box::new(NodeVariant(lhs.into()));
        }

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

trait ASTNode {
    fn execute(&self, context: &mut ExecutionContext);
}

struct NodeFunctionBlock {
    arg_names: Vec<String>,
    block: Vec<Box<dyn ASTNode>>,
}

impl ASTNode for NodeFunctionBlock {
    fn execute(&self, context: &mut ExecutionContext) {
        for instruction in self.block.iter() {
            instruction.execute(context);
        }
    }
}

impl fmt::Debug for NodeFunctionBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        f.write_str("<Function>")
    }
}

struct NodeLoop {
    condition: Box<dyn ASTNode>,
    block: Vec<Box<dyn ASTNode>>,
}

impl ASTNode for NodeLoop {
    fn execute(&self, context: &mut ExecutionContext) {
        loop {
            self.condition.execute(context);
            let condition = context.pop();

            if condition.is_false() {
                break;
            }

            for node in &self.block {
                node.execute(context);
            }
        }
    }
}

struct NodeConditional {
    condition: Box<dyn ASTNode>,
    true_block: Vec<Box<dyn ASTNode>>,
    false_block: Vec<Box<dyn ASTNode>>,
}

impl ASTNode for NodeConditional {
    fn execute(&self, context: &mut ExecutionContext) {
        self.condition.execute(context);
        let condition = context.pop();

        let executing_block = if condition.is_true() {
            &self.true_block
        } else {
            &self.false_block
        };

        for node in executing_block {
            node.execute(context)
        }
    }
}

struct NodeUnaryOperation {
    op: String,
    rhs: Box<dyn ASTNode>,
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
            print!("{}", arg);
        }
        println!();
        Vec::new()
    }

    pub fn assert(args: Vec<Variant>) -> Vec<Variant> {
        if args.len() != 1 {
            panic!("assert() expects only 1 argument");
        }

        assert!(args[0].is_true());
        Vec::new()
    }

    pub mod math {
        use crate::Variant;
        use std::convert::TryFrom;

        pub fn pow(args: Vec<Variant>) -> Vec<Variant> {
            if args.len() != 2 {
                panic!("pow(n, e) expects 2 arguments, {} given.", args.len());
            }

            let result = match (&args[0], &args[1]) {
                (Variant::Number(a), Variant::Number(b)) => {
                    if b.is_negative() {
                        panic!("pow(n, e) can't have negative exponents until the language supports floating numbers");
                    }

                    i32::pow(*a, u32::try_from(*b).unwrap())
                }
                (_, _) => panic!("pow(n, e) can only be used with numeric arguments."),
            };

            vec![Variant::Number(result)]
        }
    }
}

struct NodeAssignment {
    identifier: NodeVariant,
    expression: Box<dyn ASTNode>,
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

#[derive(Default)]
struct ExecutionContext {
    variables: HashMap<String, Variant>,
    stack: Vec<Variant>,
    functions: Vec<Rc<NodeFunctionBlock>>,
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
        self.variable_set(
            name,
            Variant::NativeFunction(NativeFunction {
                name: name.to_owned(),
                func,
            }),
        );
    }

    fn call_native_function(&mut self, name: &str, args: Vec<Variant>) {
        match self.variable_lookup(name) {
            Some(Variant::NativeFunction(f)) => {
                let results = f(args);
                for result in results.into_iter().rev() {
                    self.push(result);
                }
            }
            Some(Variant::Function(block)) => {
                let f = self.functions[(*block) as usize].clone();
                let mut function_scope: HashMap<String, Variant> = self.variables.clone();

                f.arg_names
                    .clone()
                    .into_iter()
                    .zip(args.into_iter())
                    .for_each(|(name, value)| {
                        function_scope.insert(name, value);
                    });
                f.execute(&mut ExecutionContext {
                    variables: function_scope,
                    stack: vec![],
                    functions: self.functions.clone(),
                });
            }
            None => panic!("Trying to call non existent function `{}`", &name),
            _ => {
                panic!("Trying to call variable which is non callable");
            }
        };
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
        functions: parser.functions.into_iter().map(Rc::new).collect(),
    };

    context.add_native_function("print", &helius_std::print);
    context.add_native_function("assert", &helius_std::assert);
    context.add_native_function("pow", &helius_std::math::pow);

    for stmt in program {
        stmt.execute(&mut context);
    }
}
