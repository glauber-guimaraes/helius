#![allow(dead_code)]

use std::{collections::HashMap, ops};

mod tokenizer;
use tokenizer::{Token, TokenType, Tokenizer};

const PROGRAM_SOURCE: &str = "a = 2
b = 3
c = a + b
d = a - b
e = c * d + a
f = c + d * a
g = 1 * 2 / 3

print a
print a, b
print \"Hello world\"
print c, d
print a + b, c
";

#[derive(PartialEq, PartialOrd, Debug)]
enum Precedence {
    None,
    Assignment,     // =
    Addition,       // + -
    Multiplication, // * /
    Unary,          // -n
    Primary,
}

impl ops::Add<i32> for Precedence {
    type Output = Precedence;

    fn add(self, rhs: i32) -> Self::Output {
        if rhs.abs() != 1 {
            panic!("Can only increase or decrease Precedence by 1");
        }
        if rhs == 1 {
            match self {
                Precedence::None => Precedence::Assignment,
                Precedence::Assignment => Precedence::Addition,
                Precedence::Addition => Precedence::Multiplication,
                Precedence::Multiplication => Precedence::Unary,
                Precedence::Unary => Precedence::Primary,
                Precedence::Primary => panic!("Trying to increase from highest precedence"),
            }
        } else {
            match self {
                Precedence::None => panic!("Trying to decrease from lowest precedence"),
                Precedence::Assignment => Precedence::None,
                Precedence::Addition => Precedence::Assignment,
                Precedence::Multiplication => Precedence::Addition,
                Precedence::Unary => Precedence::Multiplication,
                Precedence::Primary => Precedence::Unary,
            }
        }
    }
}

impl From<&Token> for Precedence {
    fn from(token: &Token) -> Self {
        match token.r#type {
            TokenType::Identifier => Precedence::Primary,
            TokenType::Number => Precedence::Primary,
            TokenType::String => Precedence::Primary,
            TokenType::Print => Precedence::None,
            TokenType::Assignment => Precedence::Assignment,
            TokenType::Plus => Precedence::Addition,
            TokenType::Minus => Precedence::Addition,
            TokenType::Mul => Precedence::Multiplication,
            TokenType::Div => Precedence::Multiplication,
            TokenType::Comma => Precedence::None,
            TokenType::Newline => Precedence::None,
            TokenType::Eof => Precedence::None,
        }
    }
}

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

type ParserResult = Result<(), ParserError>;

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

    fn expect(&mut self, token: TokenType, msg: &str) -> ParserResult {
        if !self.match_and_advance(token) {
            return Err(ParserError::new(
                &format!(
                    "Expected token {:?} got {:?}\n{}",
                    token,
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
            match self.parse_statement() {
                Ok(_) => (),
                Err(e) => panic!("{:?}", e),
            }

            if self.match_and_advance(TokenType::Eof) {
                break;
            }
        }
    }

    fn match_and_advance(&mut self, token: TokenType) -> bool {
        if self.current.as_ref().unwrap().is_type(token) {
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
        let ident = match self.current.as_ref().unwrap().r#type {
            TokenType::Identifier => self.current.as_ref().unwrap().lexeme.clone(),
            _ => panic!(),
        };
        self.advance();
        self.expect(TokenType::Assignment, "Expected after identifier")?;
        print!("Assignment of `{}`: ", ident);
        print!("{}", self.parse_expression(Precedence::Assignment));
        println!();
        self.expect(TokenType::Newline, "")?;
        Ok(())
    }

    fn parse_print(&mut self) -> ParserResult {
        assert!(self.match_and_advance(TokenType::Print));
        print!("Print: ");
        print!("{}", self.parse_expression(Precedence::Assignment));

        while self.match_and_advance(TokenType::Comma) {
            print!(", ");
            print!("{}", self.parse_expression(Precedence::Assignment));
        }
        println!();
        Ok(())
    }

    fn parse_expression(&mut self, precedence: Precedence) -> String {
        // prefix expression
        let mut lhs = match self.current.as_ref().unwrap().r#type {
            TokenType::Identifier | TokenType::Number | TokenType::String => {
                format!("{}", self.current.as_ref().unwrap())
            }
            _ => panic!("Expected prefix expression, found {:?}", self.current),
        };
        self.advance();

        // infix expression
        while precedence < self.current.as_ref().unwrap().into() {
            let op = self.current.as_ref().unwrap().clone();
            let new_precedence: Precedence = self.current.as_ref().unwrap().into();
            self.advance();
            let rhs = self.parse_expression(new_precedence);
            lhs = format!("[{}] {} {}", op, &lhs, &rhs);
        }
        lhs
    }

    fn consume(&mut self) -> Token {
        let t = self.current.as_ref().unwrap().clone();
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
