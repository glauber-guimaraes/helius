use std::fmt::Display;

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum TokenType {
    Identifier,
    Number,
    String,
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

#[derive(Debug, Clone)]
pub struct Token {
    pub r#type: TokenType,
    pub lexeme: String,
    pub line: usize,
    pub column: usize,
}

pub enum Precedence {
    None,
    Assignment,     // =
    Addition,       // + -
    Multiplication, // * /
    Unary,          // -n
    Primary,
}

impl Token {
    fn new(r#type: TokenType, lexeme: String, line: usize, column: usize) -> Self {
        Token {
            r#type,
            lexeme,
            line,
            column,
        }
    }

    pub fn is_type(&self, other: TokenType) -> bool {
        self.r#type == other
    }

    pub fn is_any_type(&self, types: &[TokenType]) -> bool {
        types.iter().any(|t| self.is_type(*t))
    }

    pub fn get_precedence(&self) -> u32 {
        match self.r#type {
            TokenType::Print | TokenType::Comma | TokenType::Newline | TokenType::Eof => {
                Precedence::None as u32
            }
            TokenType::Assignment => Precedence::Assignment as u32,
            TokenType::Plus | TokenType::Minus => Precedence::Addition as u32,
            TokenType::Mul | TokenType::Div => Precedence::Multiplication as u32,
            TokenType::Identifier | TokenType::Number | TokenType::String => {
                Precedence::Primary as u32
            }
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        match self.r#type {
            TokenType::Identifier => f.write_fmt(format_args!("Ident({})", &self.lexeme)),
            TokenType::Number => f.write_fmt(format_args!("N({})", &self.lexeme)),
            TokenType::String => f.write_fmt(format_args!("\"{}\"", &self.lexeme)),
            TokenType::Plus | TokenType::Minus | TokenType::Mul | TokenType::Div => {
                f.write_str(&self.lexeme)
            }
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
    UndefinedCharacter(usize, usize),
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

    fn create_token(&self, r#type: TokenType, lexeme: &str) -> Token {
        Token::new(r#type, lexeme.to_string(), self.line + 1, self.column)
    }

    pub fn next(&mut self) -> Result<Token, TokenError> {
        if self.index == self.source.len() {
            return Ok(self.create_token(TokenType::Eof, ""));
        }

        let chr = self.source[self.index];

        if chr.is_ascii_digit() {
            let lexeme = self.parse_number();
            Ok(self.create_token(TokenType::Number, &lexeme))
        } else if chr.is_ascii_whitespace() && chr == '\n' {
            self.advance();
            Ok(self.create_token(TokenType::Newline, "\n"))
        } else if chr.is_ascii_alphabetic() {
            let ident = self.parse_identifier();
            if ident == "print" {
                Ok(self.create_token(TokenType::Print, "print"))
            } else {
                Ok(self.create_token(TokenType::Identifier, &ident))
            }
        } else if chr.is_ascii_punctuation() {
            self.advance();
            match chr {
                '=' => Ok(self.create_token(TokenType::Assignment, "=")),
                '+' => Ok(self.create_token(TokenType::Plus, "+")),
                '-' => Ok(self.create_token(TokenType::Minus, "-")),
                '*' => Ok(self.create_token(TokenType::Mul, "*")),
                '/' => Ok(self.create_token(TokenType::Div, "/")),
                ',' => Ok(self.create_token(TokenType::Comma, ",")),
                '"' => {
                    let lexeme = self.parse_string();
                    Ok(self.create_token(TokenType::String, &lexeme))
                }
                _ => Err(TokenError::UndefinedCharacter(self.line, self.column)),
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

    fn parse_number(&mut self) -> String {
        let mut number_str = String::new();
        while self.source[self.index].is_ascii_alphanumeric() {
            number_str.push(self.source[self.index]);
            self.advance();
        }
        number_str
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
