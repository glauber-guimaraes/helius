#![allow(dead_code)]

use std::collections::HashMap;

mod tokenizer;
use tokenizer::{Token, TokenType, Tokenizer};

use crate::tokenizer::Precedence;

const PROGRAM_SOURCE: &str = "a = 2
b = 3
c = a + b
d = a - b
e = c * d + a
f = c + d * a
g = 1 * 2 / 3
print a
print a, b
+
print c, d +

print a + b, c +";

struct Parser {
    tokenizer: Tokenizer,
    current: Option<Token>,
}

#[derive(Debug)]
struct ParserError {
    msg: String,
    line: usize,
    column: usize,
}

impl ParserError {
    fn new(msg: &str, line: usize, column: usize) -> Self {
        Self {
            msg: msg.to_string(),
            line,
            column,
        }
    }
}

type ParserResult<T = ()> = Result<T, ParserError>;

impl Parser {
    fn new(tokenizer: Tokenizer) -> Self {
        Parser {
            tokenizer,
            current: None,
        }
    }

    fn parse(&mut self) {
        self.program();
    }

    fn expect(&mut self, token_type: TokenType, msg: &str) -> ParserResult {
        if !self.match_and_advance(token_type) {
            return Err(ParserError::new(
                &format!(
                    "Expected token {:?} got {:?}\n{}",
                    token_type,
                    self.current.as_ref().unwrap(),
                    msg
                ),
                self.current.as_ref().unwrap().line,
                self.current.as_ref().unwrap().column,
            ));
        }
        Ok(())
    }

    fn program(&mut self) {
        self.advance();
        loop {
            while self.match_and_advance(TokenType::Newline) {}

            if self.match_and_advance(TokenType::Eof) {
                break;
            }

            match self.parse_statement() {
                Ok(_) => (),
                Err(e) => {
                    println!("\n{}:{}: error: {}\n", e.line, e.column, e.msg);
                    self.recover_from_error();
                }
            }
        }
    }

    fn recover_from_error(&mut self) {
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

    fn parse_statement(&mut self) -> ParserResult {
        match self.current.as_ref().unwrap().r#type {
            TokenType::Identifier => self.parse_assignment(),
            TokenType::Print => self.parse_print(),
            _ => Err(ParserError::new(
                &format!(
                    "Unexpected statement start `{}`",
                    &self.current.as_ref().unwrap()
                ),
                self.current.as_ref().unwrap().line,
                self.current.as_ref().unwrap().column,
            )),
        }
    }

    fn parse_assignment(&mut self) -> ParserResult {
        assert!(self.current.as_ref().unwrap().r#type == TokenType::Identifier);
        let ident = self.consume();
        self.expect(TokenType::Assignment, "Expected after identifier")?;

        let expression = self.parse_expression(Precedence::Assignment as u32)?;
        println!("{} = {}", ident, expression);
        Ok(())
    }

    fn parse_print(&mut self) -> ParserResult {
        assert!(self.match_and_advance(TokenType::Print));
        print!("Print: ");
        let expression = self.parse_expression(Precedence::Assignment as u32)?;
        print!("{}", expression);

        while self.match_and_advance(TokenType::Comma) {
            print!(", ");
            let expression = self.parse_expression(Precedence::Assignment as u32)?;
            print!("{}", expression);
        }
        println!();
        Ok(())
    }

    fn create_error_at_token<T>(&self, token: &Token, msg: &str) -> ParserResult<T> {
        Err(ParserError::new(msg, token.line, token.column))
    }

    fn parse_expression(&mut self, precedence: u32) -> ParserResult<String> {
        // prefix expression
        let lhs = self.consume();
        if !lhs.is_any_type(&[TokenType::Identifier, TokenType::Number, TokenType::String]) {
            return self
                .create_error_at_token(&lhs, &format!("Expected expression, found `{}`", &lhs));
        }
        let mut expression = format!("{}", lhs);

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
                );
            }
            let rhs = match self.parse_expression(op.get_precedence()) {
                Ok(expr) => expr,
                Err(err) => {
                    return self.create_error_at_token(
                        &op,
                        &format!("{}. Expected after `{}` token.", err.msg, op.lexeme),
                    );
                }
            };
            expression = format!("[{}] {} {}", op, &expression, &rhs);
        }
        Ok(expression)
    }

    fn consume(&mut self) -> Token {
        let t = self.current.to_owned().unwrap();
        self.advance();
        t
    }
}

#[derive(Debug, Clone)]
enum Variant {
    Identifier(String),
    String(String),
    Number(i32),
}

impl From<Token> for Variant {
    fn from(token: Token) -> Self {
        match token.r#type {
            TokenType::Identifier => todo!(),
            TokenType::Number => todo!(),
            TokenType::String => todo!(),
            _ => panic!("Invalid conversion for token {:?}", token),
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

fn main() {
    let tokenizer = Tokenizer::new(PROGRAM_SOURCE.to_owned());
    let mut parser = Parser::new(tokenizer);

    parser.parse();
}
