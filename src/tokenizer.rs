use std::fmt::Display;

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum TokenType {
    Identifier,
    Number,
    Float,
    String,
    Boolean,
    Assignment,
    Plus,
    Minus,
    Mul,
    Div,
    LowerThan,
    LowerOrEqualThan,
    GreaterThan,
    GreaterOrEqualThan,
    Equality,
    Inequality,
    LeftParenthesis,
    RightParenthesis,
    If,
    Then,
    Else,
    End,
    While,
    Do,
    None,
    Function,
    Break,
    Continue,
    Return,
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
    Parenthesis,    // ( )
    Assignment,     // =
    Relational,     // < > <= >= == !=
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
            TokenType::Comma | TokenType::Newline | TokenType::Eof => Precedence::None as u32,
            TokenType::Assignment => Precedence::Assignment as u32,
            TokenType::Plus | TokenType::Minus => Precedence::Addition as u32,
            TokenType::Mul | TokenType::Div => Precedence::Multiplication as u32,
            TokenType::GreaterThan
            | TokenType::GreaterOrEqualThan
            | TokenType::LowerThan
            | TokenType::LowerOrEqualThan
            | TokenType::Equality
            | TokenType::Inequality => Precedence::Relational as u32,
            TokenType::Identifier | TokenType::Number | TokenType::String | TokenType::Boolean => {
                Precedence::Primary as u32
            }
            TokenType::LeftParenthesis | TokenType::RightParenthesis => {
                Precedence::Parenthesis as u32
            }
            _ => Precedence::None as u32,
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
            TokenType::Newline => f.write_str("\\n"),
            TokenType::Eof => f.write_str("end of file"),
            _ => f.write_fmt(format_args!("{}", self.lexeme)),
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
        Token::new(r#type, lexeme.to_string(), self.line + 1, self.column + 1)
    }

    pub fn next(&mut self) -> Result<Token, TokenError> {
        if self.index == self.source.len() {
            return Ok(self.create_token(TokenType::Eof, ""));
        }

        let chr = self.source[self.index];

        if chr.is_ascii_digit() {
            let token = self.parse_number();
            Ok(token)
        } else if chr.is_ascii_whitespace() && chr == '\n' {
            let result = self.create_token(TokenType::Newline, "\n");
            self.advance();
            Ok(result)
        } else if chr.is_ascii_alphabetic() {
            let mut token = self.parse_identifier();
            token.r#type = match &*token.lexeme {
                "true" | "false" => TokenType::Boolean,
                "if" => TokenType::If,
                "then" => TokenType::Then,
                "else" => TokenType::Else,
                "end" => TokenType::End,
                "while" => TokenType::While,
                "do" => TokenType::Do,
                "none" => TokenType::None,
                "function" => TokenType::Function,
                "break" => TokenType::Break,
                "continue" => TokenType::Continue,
                "return" => TokenType::Return,
                _ => token.r#type,
            };
            Ok(token)
        } else if chr.is_ascii_punctuation() {
            let result = match chr {
                '+' => Ok(self.create_token(TokenType::Plus, "+")),
                '-' => Ok(self.create_token(TokenType::Minus, "-")),
                '*' => Ok(self.create_token(TokenType::Mul, "*")),
                '/' => Ok(self.create_token(TokenType::Div, "/")),
                ',' => Ok(self.create_token(TokenType::Comma, ",")),
                '<' | '>' | '=' | '!' => self.parse_relational_token(chr),
                '(' => Ok(self.create_token(TokenType::LeftParenthesis, "(")),
                ')' => Ok(self.create_token(TokenType::RightParenthesis, ")")),
                '"' => Ok(self.parse_string()),
                '#' => {
                    self.consume_comment();
                    return self.next();
                }
                _ => Err(TokenError::UndefinedCharacter(self.line, self.column)),
            };
            self.advance();
            result
        } else {
            self.advance();
            self.next()
        }
    }

    fn parse_relational_token(&mut self, start_char: char) -> Result<Token, TokenError> {
        self.advance();

        if !self.is_eof() && self.source[self.index] == '=' {
            return match start_char {
                '>' => Ok(self.create_token(TokenType::GreaterOrEqualThan, ">=")),
                '<' => Ok(self.create_token(TokenType::LowerOrEqualThan, "<=")),
                '=' => Ok(self.create_token(TokenType::Equality, "==")),
                '!' => Ok(self.create_token(TokenType::Inequality, "!=")),
                _ => Err(TokenError::UndefinedCharacter(self.line, self.column)),
            };
        }

        match start_char {
            '>' => Ok(self.create_token(TokenType::GreaterThan, ">")),
            '<' => Ok(self.create_token(TokenType::LowerThan, "<")),
            '=' => Ok(self.create_token(TokenType::Assignment, "=")),
            '!' => Err(TokenError::UndefinedCharacter(self.line, self.column)),
            _ => unreachable!(),
        }
    }

    fn advance(&mut self) {
        if self.index >= self.source.len() {
            return;
        }

        let chr = self.source[self.index];
        self.index += 1;
        self.column += 1;

        if chr == '\n' {
            self.line += 1;
            self.column = 0;
        }
    }

    fn is_eof(&self) -> bool {
        self.index >= self.source.len()
    }

    fn parse_number(&mut self) -> Token {
        let mut token = self.create_token(TokenType::Number, "");
        let mut number_str = String::new();
        while !self.is_eof() && self.source[self.index].is_ascii_digit() {
            number_str.push(self.source[self.index]);
            self.advance();
        }
        if !self.is_eof() && self.source[self.index] == '.' {
            self.advance();
            number_str.push('.');
            token.r#type = TokenType::Float;

            while !self.is_eof() && self.source[self.index].is_ascii_digit() {
                number_str.push(self.source[self.index]);
                self.advance();
            }
        }
        token.lexeme = number_str;
        token
    }

    fn parse_identifier(&mut self) -> Token {
        let mut token = self.create_token(TokenType::Identifier, "");
        let mut ident = String::new();

        while !self.is_eof()
            && (self.source[self.index].is_ascii_alphanumeric() || self.source[self.index] == '_')
        {
            let chr = self.source[self.index];
            ident.push(chr);
            self.advance();
        }
        token.lexeme = ident;
        token
    }

    fn parse_string(&mut self) -> Token {
        self.advance();
        let mut token = self.create_token(TokenType::String, "");
        let mut str = String::new();

        while self.source[self.index] != '"' {
            str.push(self.source[self.index]);
            self.advance();
        }
        token.lexeme = str;
        token
    }

    fn consume_comment(&mut self) {
        while self.source[self.index] != '\n' {
            self.advance();
        }
    }

    pub fn get_source_line(&self, line: usize) -> String {
        self.source
            .iter()
            .collect::<String>()
            .split('\n')
            .into_iter()
            .nth(line - 1)
            .unwrap()
            .to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tokenizer_returns_number() {
        let program = r#"1"#;
        test_tokenizer_returns_correct_type(program, TokenType::Number);
        test_tokenizer_returns_correct_lexeme(program, "1");
    }

    #[test]
    fn tokenizer_returns_string() {
        let program = r#""hello world""#;
        test_tokenizer_returns_correct_type(program, TokenType::String);
        test_tokenizer_returns_correct_lexeme(program, "hello world");
    }

    fn test_tokenizer_returns_correct_type(program: &str, expected_type: TokenType) {
        let mut tokenizer = Tokenizer::new(program.to_string());
        let token = tokenizer.next().unwrap();

        assert_eq!(token.r#type, expected_type);
    }

    fn test_tokenizer_returns_correct_lexeme(program: &str, expected: &str) {
        let mut tokenizer = Tokenizer::new(program.to_string());
        let token = tokenizer.next().unwrap();

        assert_eq!(token.lexeme, expected);
    }

    #[test]
    fn test_comma_after_string() {
        let program = "\"x\",x";
        let mut tokenizer = Tokenizer::new(program.to_string());

        let expected = &[TokenType::String, TokenType::Comma, TokenType::Identifier];

        let mut result = vec![];
        loop {
            if let Ok(token) = tokenizer.next() {
                if token.is_type(TokenType::Eof) {
                    break;
                }

                result.push(token);
            } else {
                panic!();
            }
        }

        assert_eq!(result.len(), expected.len());
        for (res, exp) in result.iter().zip(expected.iter()) {
            assert!(res.is_type(*exp));
        }
    }
}
