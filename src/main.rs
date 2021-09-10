#![allow(dead_code)]

use std::{collections::HashMap, mem};

mod tokenizer;
use tokenizer::{Token, Tokenizer};

const PROGRAM_SOURCE: &str = "a = 2
b = 3 4
c = a + b
d = a - b

print a, b
print \"Hello world\"
print c, d
";

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
        /*
         * program ::= {Newline | stmt} end
         * stmt ::= assignment | print
         * assignment ::= Identifier '=' expr
         * print ::= Print expr [exprlist]
         * exprlist ::= Comma expr [exprlist]
         * expr ::= (Identifier | Number) [binary_op expr]
         * binary_op ::= '+' | '-'
         * end ::= EOF
         */
        self.program();
    }

    fn print(&mut self) {
        loop {
            let token = self.tokenizer.next();
            match token {
                Ok(Token::Eof) => {
                    println!("--- EOF ---");
                    break;
                }
                Ok(t) => println!("{:?}", t),
                Err(e) => {
                    println!("Error!\n    {:?}", &e);
                    break;
                }
            };
        }
    }

    fn expect(&mut self, token: Token, msg: &str) {
        let next_token = self.tokenizer.peek().unwrap();
        if mem::discriminant(&next_token) != mem::discriminant(&token) {
            println!("Expected token {:?} got {:?}\n{}", token, next_token, msg);
            panic!();
        }
    }

    fn program(&mut self) {
        loop {
            while self.match_and_advance(Token::Newline) {}
            self.parse_statement();

            if self.match_and_advance(Token::Eof) {
                break;
            }

            self.expect(Token::Newline, "");
        }
    }

    fn match_and_advance(&mut self, token: Token) -> bool {
        let next = self.tokenizer.peek().unwrap();
        if mem::discriminant(&next) == mem::discriminant(&token) {
            self.advance();
            return true;
        }
        false
    }

    fn advance(&mut self) {
        self.current = self.tokenizer.next().ok();
    }

    fn parse_statement(&mut self) {
        match self.tokenizer.peek().unwrap() {
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
        let ident = match self.tokenizer.next().unwrap() {
            Token::Identifier(t) => t,
            _ => panic!(),
        };

        assert!(
            self.match_and_advance(Token::Assignment),
            "Expected `=` after identifier"
        );
        print!("Assignment of `{}` :", ident);
        self.parse_expression();
        println!();
    }

    fn parse_print(&mut self) {
        assert!(self.match_and_advance(Token::Print));
        print!("Print :");
        self.parse_expression();

        while self.match_and_advance(Token::Comma) {
            self.parse_expression();
        }
        println!();
    }

    fn parse_expression(&mut self) {
        self.advance();
        let action = match self.current.as_ref().unwrap() {
            Token::Identifier(ident) => format!("Expr({}) ", ident),
            Token::Number(n) => format!("Number({}) ", n),
            Token::String(s) => format!("Str({}) ", s),
            _ => panic!(),
        };

        if self.match_and_advance(Token::Plus) || self.match_and_advance(Token::Minus) {
            print!("{:?} {}", self.current.as_ref().unwrap(), &action);
            self.parse_expression();
        } else {
            print!("{}", &action);
        }
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
