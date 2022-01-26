use std::process;

use crate::{
    tokenizer::{Precedence, Token, TokenType, Tokenizer},
    variant::Variant,
    ASTNode, NodeAssignment, NodeBinaryOperation, NodeBreak, NodeCall, NodeConditional,
    NodeContinue, NodeExpressionList, NodeFunctionBlock, NodeLoop, NodeReturn, NodeUnaryOperation,
    NodeVariant,
};

pub struct Parser {
    tokenizer: Tokenizer,
    current: Option<Token>,
    has_error: bool,
    pub functions: Vec<NodeFunctionBlock>,
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
    pub fn new(tokenizer: Tokenizer) -> Self {
        Parser {
            tokenizer,
            current: None,
            has_error: false,
            functions: vec![],
        }
    }

    pub fn parse(&mut self) -> Vec<Box<dyn ASTNode>> {
        self.advance();
        let block = self.parse_block();
        assert_eq!(
            self.peek_type(),
            TokenType::Eof,
            "parser should stop at eof"
        );
        block
    }

    fn parse_block(&mut self) -> Vec<Box<dyn ASTNode>> {
        let mut statements: Vec<Box<dyn ASTNode>> = vec![];

        loop {
            while self.match_and_advance(TokenType::Newline) {}

            if self.match_and_advance(TokenType::Eof) {
                break;
            }

            match self.try_parse_statement() {
                Some(Ok(stmt)) => statements.push(stmt),
                Some(Err(e)) => {
                    self.print_error(e);
                    self.recover_from_error();
                }
                None => {
                    break;
                }
            }
        }

        if self.has_error {
            println!("error: can't compile program due to previous errors");
            process::exit(1);
        }

        statements
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

    fn try_parse_statement(&mut self) -> Option<ParserResult<Box<dyn ASTNode>>> {
        let current_type = self.peek_type();

        if [TokenType::If, TokenType::While].contains(&current_type) {
            let result = match current_type {
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

            return Some(result);
        }

        if current_type == TokenType::Return {
            return Some(self.parse_return());
        }

        if current_type == TokenType::Continue {
            self.advance();
            return Some(Ok(Box::new(NodeContinue)));
        }

        if current_type == TokenType::Break {
            self.advance();
            return Some(Ok(Box::new(NodeBreak)));
        }

        if current_type != TokenType::Identifier {
            return None;
        }

        let ident = self.consume();
        if self.match_and_advance(TokenType::Assignment) {
            return Some(self.parse_assignment(ident));
        }

        let current_type = self.current.as_ref().unwrap().r#type;
        if current_type == TokenType::String || current_type == TokenType::LeftParenthesis {
            return Some(self.parse_function_call(ident, Some(0)));
        }

        unreachable!(
            "if control reaches here, we didn't properly detect an error. Current token is {:?}",
            ident
        );
    }

    fn parse_loop(&mut self) -> ParserResult<Box<dyn ASTNode>> {
        self.expect(TokenType::While, "")?;
        let condition = self.parse_expression(Precedence::Assignment as u32)?;
        self.expect(TokenType::Do, "")?;
        let block = self.parse_block();
        self.expect(TokenType::End, "a loop must end with an `end`")?;

        Ok(Box::new(NodeLoop { condition, block }))
    }

    fn parse_conditional(&mut self) -> ParserResult<Box<dyn ASTNode>> {
        self.expect(TokenType::If, "")?;
        let condition = self.parse_expression(Precedence::Assignment as u32)?;
        self.expect(TokenType::Then, "")?;

        let true_block = self.parse_block();
        let mut false_block = vec![];
        if self.match_and_advance(TokenType::Else) {
            if self.peek_type() == TokenType::If {
                false_block.push(self.parse_conditional()?);
            } else {
                false_block = self.parse_block();
            }
        }
        self.match_and_advance(TokenType::End);

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

    fn parse_function_call(
        &mut self,
        ident: Token,
        expected_return_count: Option<usize>,
    ) -> ParserResult<Box<dyn ASTNode>> {
        if self.current.as_ref().unwrap().is_type(TokenType::String) {
            let arg = self.consume();
            let mut args = NodeExpressionList(vec![]);
            args.push(Box::new(NodeVariant(arg.into())));
            return Ok(Box::new(NodeCall {
                func: ident.lexeme,
                args,
                expected_return_count,
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
            expected_return_count,
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

        let mut block = self.parse_block();
        self.expect(
            TokenType::End,
            "a function definition must end with an `end`",
        )?;

        block.push(Box::new(NodeReturn {
            args: NodeExpressionList(vec![]),
        }));

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
            expr_node = self.parse_function_call(lhs, Some(1))?;
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

    fn parse_return(&mut self) -> ParserResult<Box<dyn ASTNode>> {
        self.expect(TokenType::Return, "")?;
        let mut args = NodeExpressionList(vec![]);

        if self.peek_type() != TokenType::Newline {
            args.push(self.parse_expression(Precedence::Assignment as u32)?);

            while self.match_and_advance(TokenType::Comma) {
                args.push(self.parse_expression(Precedence::Assignment as u32)?);
            }
        }

        Ok(Box::new(NodeReturn { args }))
    }
}
