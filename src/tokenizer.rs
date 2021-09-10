use std::fmt::Display;

#[derive(Debug, Clone)]
pub enum Token {
    Identifier(String),
    Number(i32),
    String(String),
    Print,
    Assignment,
    Plus,
    Minus,
    Mul,
    Div,
    Comma,
    Newline,
    Eof,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        match self {
            Token::Identifier(i) => f.write_fmt(format_args!("Ident({})", &i)),
            Token::Number(i) => f.write_fmt(format_args!("N({})", i)),
            Token::String(s) => f.write_fmt(format_args!("\"{}\"", s)),
            Token::Plus => f.write_str("+"),
            Token::Minus => f.write_str("-"),
            _ => f.write_fmt(format_args!("{:?}", self)),
        }
    }
}

pub struct Tokenizer {
    source: Vec<char>,
    index: usize,
    line: usize,
    column: usize,
}

#[derive(Debug)]
pub enum TokenError {
    Generic(usize, usize),
}

impl Tokenizer {
    pub fn new(source: String) -> Self {
        Tokenizer {
            source: source.chars().collect(),
            index: 0,
            line: 0,
            column: 0,
        }
    }

    pub fn next(&mut self) -> Result<Token, TokenError> {
        if self.index == self.source.len() {
            return Ok(Token::Eof);
        }

        let chr = self.source[self.index];

        if chr.is_ascii_digit() {
            Ok(Token::Number(self.parse_number()))
        } else if chr.is_ascii_whitespace() && chr == '\n' {
            self.advance();
            Ok(Token::Newline)
        } else if chr.is_ascii_alphabetic() {
            let ident = self.parse_identifier();
            if ident == "print" {
                Ok(Token::Print)
            } else {
                Ok(Token::Identifier(ident))
            }
        } else if chr.is_ascii_punctuation() {
            self.advance();
            match chr {
                '=' => Ok(Token::Assignment),
                '+' => Ok(Token::Plus),
                '-' => Ok(Token::Minus),
                '*' => Ok(Token::Mul),
                '/' => Ok(Token::Div),
                ',' => Ok(Token::Comma),
                '"' => Ok(Token::String(self.parse_string())),
                _ => Err(TokenError::Generic(self.line, self.column)),
            }
        } else {
            self.advance();
            self.next()
        }
    }

    fn advance(&mut self) {
        let chr = self.source[self.index];
        self.index += 1;
        self.column += 1;

        if chr == '\n' {
            self.line += 1;
            self.column = 0;
        }
    }

    fn parse_number(&mut self) -> i32 {
        let mut number_str = String::new();
        while self.source[self.index].is_ascii_alphanumeric() {
            number_str.push(self.source[self.index]);
            self.advance();
        }
        number_str.parse::<i32>().unwrap()
    }

    fn parse_identifier(&mut self) -> String {
        let mut ident = String::new();

        while self.source[self.index].is_ascii_alphanumeric() {
            let chr = self.source[self.index];
            ident.push(chr);
            self.advance();
        }
        ident
    }

    fn parse_string(&mut self) -> String {
        let mut str = String::new();

        while self.source[self.index] != '"' {
            str.push(self.source[self.index]);
            self.advance();
        }
        // Skip the ending `"`
        self.advance();
        str
    }
}
