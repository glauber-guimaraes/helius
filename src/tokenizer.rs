#[derive(Debug, Clone)]
pub enum Token {
    Identifier(String),
    Number(i32),
    String(String),
    Print,
    Assignment,
    Plus,
    Minus,
    Comma,
    Newline,
    Eof,
}

pub struct Tokenizer {
    source: Vec<char>,
    index: usize,
}

#[derive(Debug)]
pub enum TokenError {
    Generic,
}

impl Tokenizer {
    pub fn new(source: String) -> Self {
        Tokenizer {
            source: source.chars().collect(),
            index: 0,
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
            self.index += 1;
            Ok(Token::Newline)
        } else if chr.is_ascii_alphabetic() {
            let ident = self.parse_identifier();
            if ident == "print" {
                Ok(Token::Print)
            } else {
                Ok(Token::Identifier(ident))
            }
        } else if chr.is_ascii_punctuation() {
            self.index += 1;
            match chr {
                '=' => Ok(Token::Assignment),
                '+' => Ok(Token::Plus),
                '-' => Ok(Token::Minus),
                ',' => Ok(Token::Comma),
                '"' => Ok(Token::String(self.parse_string())),
                _ => Err(TokenError::Generic),
            }
        } else {
            self.index += 1;
            self.next()
        }
    }

    pub fn peek(&mut self) -> Result<Token, TokenError> {
        let index = self.index;
        let token = self.next();
        self.index = index;
        token
    }

    fn parse_number(&mut self) -> i32 {
        let mut number_str = String::new();
        while self.source[self.index].is_ascii_alphanumeric() {
            number_str.push(self.source[self.index]);
            self.index += 1;
        }
        number_str.parse::<i32>().unwrap()
    }

    fn parse_identifier(&mut self) -> String {
        let mut ident = String::new();

        while self.source[self.index].is_ascii_alphanumeric() {
            let chr = self.source[self.index];
            ident.push(chr);
            self.index += 1;
        }
        ident
    }

    fn parse_string(&mut self) -> String {
        let mut str = String::new();

        while self.source[self.index] != '"' {
            str.push(self.source[self.index]);
            self.index += 1;
        }
        // Skip the ending `"`
        self.index += 1;
        str
    }
}
