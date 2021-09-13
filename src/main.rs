#![allow(dead_code)]

use core::fmt;
use std::collections::HashMap;

mod tokenizer;
use tokenizer::{Token, TokenType, Tokenizer};

use crate::tokenizer::Precedence;

const PROGRAM_SOURCE: &str = "a = 2
b = 3
c = a + b
d = a - b
# line comment
e = c * d + a # mid-line comment
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
        }
    }

    fn parse(&mut self) {
        self.program();
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
                    println!("\n{}:{}: error: {}", e.line, e.column, e.msg);
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
}

trait ASTNode: fmt::Display {}

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

impl ASTNode for NodeBinaryOperation {}

struct NodeVariant(Variant);
impl fmt::Display for NodeVariant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{:?}", self.0))
    }
}
impl ASTNode for NodeVariant {}

#[derive(Debug, Clone)]
enum Variant {
    Identifier(String),
    String(String),
    Number(i32),
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
