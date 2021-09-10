#![allow(dead_code)]

use std::{collections::HashMap, mem, ops};

mod tokenizer;
use tokenizer::{Token, Tokenizer};

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

impl From<Token> for Precedence {
    fn from(token: Token) -> Self {
        match token {
            Token::Identifier(_) => Precedence::Primary,
            Token::Number(_) => Precedence::Primary,
            Token::String(_) => Precedence::Primary,
            Token::Print => Precedence::None,
            Token::Assignment => Precedence::Assignment,
            Token::Plus => Precedence::Addition,
            Token::Minus => Precedence::Addition,
            Token::Mul => Precedence::Multiplication,
            Token::Div => Precedence::Multiplication,
            Token::Comma => Precedence::None,
            Token::Newline => Precedence::None,
            Token::Eof => Precedence::None,
        }
    }
}

struct Parser {
    tokenizer: Tokenizer,
    current: Option<Token>,
}

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

    fn expect(&mut self, token: Token, msg: &str) {
        //let next_token = self.tokenizer.peek().unwrap();
        if !self.match_and_advance(&token) {
            println!(
                "Expected token {:?} got {:?}\n{}",
                token,
                self.current.as_ref().unwrap(),
                msg
            );
            panic!();
        }
    }

    fn program(&mut self) {
        self.advance();
        loop {
            while self.match_and_advance(&Token::Newline) {}
            self.parse_statement();

            if self.match_and_advance(&Token::Eof) {
                break;
            }
        }
    }

    fn match_and_advance(&mut self, token: &Token) -> bool {
        //let next = self.tokenizer.peek().unwrap();
        if mem::discriminant(self.current.as_ref().unwrap()) == mem::discriminant(&token) {
            self.advance();
            return true;
        }
        false
    }

    fn advance(&mut self) {
        self.current = self.tokenizer.next().ok();
    }

    fn parse_statement(&mut self) {
        match self.current.as_ref().unwrap() {
            Token::Identifier(_) => {
                self.parse_assignment();
            }
            Token::Print => {
                self.parse_print();
            }
            _ => (),
        }
    }

    fn parse_assignment(&mut self) {
        let ident = match self.current.as_ref().unwrap() {
            Token::Identifier(t) => t.clone(),
            _ => panic!(),
        };
        self.advance();
        self.expect(Token::Assignment, "Expected after identifier");
        print!("Assignment of `{}`: ", ident);
        print!("{}", self.parse_expression(Precedence::Assignment));
        self.expect(Token::Newline, "");
        println!();
    }

    fn parse_print(&mut self) {
        assert!(self.match_and_advance(&Token::Print));
        print!("Print: ");
        print!("{}", self.parse_expression(Precedence::Assignment));

        while self.match_and_advance(&Token::Comma) {
            print!(", ");
            print!("{}", self.parse_expression(Precedence::Assignment));
        }
        println!();
    }

    fn parse_expression(&mut self, precedence: Precedence) -> String {
        // prefix expression
        let mut lhs = match self.current.as_ref().unwrap() {
            Token::Identifier(_) | Token::Number(_) | Token::String(_) => {
                format!("{}", self.current.as_ref().unwrap())
            }
            _ => panic!("Expected prefix expression, found {:?}", self.current),
        };
        self.advance();

        // infix expression
        while precedence < self.current.as_ref().unwrap().clone().into() {
            let op = self.current.as_ref().unwrap().clone();
            let new_precedence: Precedence = self.current.as_ref().unwrap().clone().into();
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
        match token {
            Token::Identifier(_) => todo!(),
            Token::Number(_) => todo!(),
            Token::String(_) => todo!(),
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
