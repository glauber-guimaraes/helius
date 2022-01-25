#![allow(dead_code)]

use std::{
    collections::HashMap,
    env::{self},
    fmt, fs, iter, process,
    rc::Rc,
    time::Instant,
};

mod tokenizer;
use tokenizer::{Precedence, Token, TokenType, Tokenizer};

mod variant;
use variant::{NativeFunction, Variant};

struct Parser {
    tokenizer: Tokenizer,
    current: Option<Token>,
    has_error: bool,
    functions: Vec<NodeFunctionBlock>,
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
            has_error: false,
            functions: vec![],
        }
    }

    fn parse(&mut self) -> Vec<Box<dyn ASTNode>> {
        self.program()
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

    fn program(&mut self) -> Vec<Box<dyn ASTNode>> {
        let mut statements: Vec<Box<dyn ASTNode>> = vec![];

        self.advance();
        loop {
            while self.match_and_advance(TokenType::Newline) {}

            if self.match_and_advance(TokenType::Eof) {
                break;
            }

            match self.parse_statement() {
                Ok(stmt) => statements.push(stmt),
                Err(e) => {
                    self.print_error(e);
                    self.recover_from_error();
                }
            }
        }

        if self.has_error {
            println!("error: can't compile program due to previous errors");
            process::exit(1);
        }

        statements
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

    /// A statement can be either an assignment or function call followed by a newline:
    /// statement ::= (<assignment> | <funccall>) "\n"
    /// assignment ::= <identifier> "=" <expression>
    /// funccall ::= <identifier> (<string_literal> | "(" <expression> {"," <expression>} ")")
    fn parse_statement(&mut self) -> ParserResult<Box<dyn ASTNode>> {
        let ident = self.consume();

        if ident.is_any_type(&[TokenType::If, TokenType::While]) {
            let result = match ident.r#type {
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

            return result;
        }

        if ident.is_type(TokenType::Return) {
            return self.parse_return();
        }

        if ident.is_type(TokenType::Continue) {
            return Ok(Box::new(NodeContinue {}));
        }

        if ident.is_type(TokenType::Break) {
            return Ok(Box::new(NodeBreak {}));
        }

        if !ident.is_type(TokenType::Identifier) {
            return self.create_error_at_token(
                &ident,
                &format!("Unexpected statement start `{}`", ident),
                "",
            );
        }

        if self.match_and_advance(TokenType::Assignment) {
            return self.parse_assignment(ident);
        }

        let current_type = self.current.as_ref().unwrap().r#type;
        if current_type == TokenType::String || current_type == TokenType::LeftParenthesis {
            return self.parse_function_call(ident, Some(0));
        }

        return self.create_error_at_token(
            self.current.as_ref().unwrap(),
            &format!(
                "Expected assignment or function call, found `{}`",
                self.current.as_ref().unwrap()
            ),
            "expected here",
        );
    }

    fn parse_loop(&mut self) -> ParserResult<Box<dyn ASTNode>> {
        let condition = self.parse_expression(Precedence::Assignment as u32)?;
        self.expect(TokenType::Do, "")?;

        let mut block: Vec<Box<dyn ASTNode>> = vec![];

        loop {
            while self.match_and_advance(TokenType::Newline) {}

            if self.match_and_advance(TokenType::Eof) {
                break;
            }

            if self.match_and_advance(TokenType::End) {
                break;
            }

            match self.parse_statement() {
                Ok(stmt) => block.push(stmt),
                Err(e) => {
                    self.print_error(e);
                    self.recover_from_error();
                }
            }
        }

        Ok(Box::new(NodeLoop { condition, block }))
    }

    fn parse_conditional(&mut self) -> ParserResult<Box<dyn ASTNode>> {
        let condition = self.parse_expression(Precedence::Assignment as u32)?;
        self.expect(TokenType::Then, "")?;

        let mut true_block = vec![];
        let mut false_block = vec![];
        let mut parsing_block = &mut true_block;

        let mut has_else_block = false;

        loop {
            while self.match_and_advance(TokenType::Newline) {}

            if self.match_and_advance(TokenType::Eof) {
                break;
            }

            if self.match_and_advance(TokenType::End) {
                break;
            }

            if self.current.as_ref().unwrap().is_type(TokenType::Else) {
                let else_token = self.current.to_owned().unwrap();
                self.advance();

                match (has_else_block, self.current.as_ref().unwrap().r#type) {
                    (true, TokenType::If) => {
                        return self.create_error_at_token(
                            &else_token,
                            "Conditional block cannot have `else if` defined after `else`",
                            "branch started here",
                        )
                    }
                    (true, _) => {
                        return self.create_error_at_token(
                            &else_token,
                            "Conditional block cannot have multiple `else` branches",
                            "branch started here",
                        )
                    }
                    (_, _) => {}
                };

                if self.match_and_advance(TokenType::If) {
                    let elseif = self.parse_conditional()?;
                    false_block.push(elseif);
                    break;
                }
                has_else_block = true;
                parsing_block = &mut false_block;
                continue;
            }

            match self.parse_statement() {
                Ok(stmt) => parsing_block.push(stmt),
                Err(e) => {
                    self.print_error(e);
                    self.recover_from_error();
                }
            }
        }

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

        let mut block = vec![];

        loop {
            while self.match_and_advance(TokenType::Newline) {}

            if self.match_and_advance(TokenType::Eof) {
                return Err(ParserError {
                    msg: "Reached end of file while parsing function definition".to_string(),
                    short_msg: "".to_string(),
                    line: 0,
                    column: 0,
                });
            }

            if self.match_and_advance(TokenType::End) {
                break;
            }

            match self.parse_statement() {
                Ok(stmt) => block.push(stmt),
                Err(e) => {
                    self.print_error(e);
                    self.recover_from_error();
                }
            }
        }

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

#[derive(PartialEq, Debug)]
enum ContinuationFlow {
    Normal,
    Return,
    Break,
    Continue,
}

trait ASTNode {
    fn execute(&self, context: &mut ExecutionContext) -> ContinuationFlow;
}

struct NodeContinue;

impl ASTNode for NodeContinue {
    fn execute(&self, _context: &mut ExecutionContext) -> ContinuationFlow {
        ContinuationFlow::Continue
    }
}

struct NodeBreak;

impl ASTNode for NodeBreak {
    fn execute(&self, _context: &mut ExecutionContext) -> ContinuationFlow {
        ContinuationFlow::Break
    }
}

struct NodeReturn {
    args: NodeExpressionList,
}

impl ASTNode for NodeReturn {
    fn execute(&self, context: &mut ExecutionContext) -> ContinuationFlow {
        self.args.execute(context);
        context.push(Variant::Number(self.args.0.len() as i32));
        ContinuationFlow::Return
    }
}

struct NodeFunctionBlock {
    arg_names: Vec<String>,
    block: Vec<Box<dyn ASTNode>>,
}

impl ASTNode for NodeFunctionBlock {
    fn execute(&self, context: &mut ExecutionContext) -> ContinuationFlow {
        for instruction in self.block.iter() {
            if instruction.execute(context) == ContinuationFlow::Return {
                break;
            }
        }

        ContinuationFlow::Normal
    }
}

impl fmt::Debug for NodeFunctionBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        f.write_str("<Function>")
    }
}

struct NodeLoop {
    condition: Box<dyn ASTNode>,
    block: Vec<Box<dyn ASTNode>>,
}

impl ASTNode for NodeLoop {
    fn execute(&self, context: &mut ExecutionContext) -> ContinuationFlow {
        'outer: loop {
            self.condition.execute(context);
            let condition = context.pop();

            if condition.is_false() {
                break;
            }

            'inner: for node in &self.block {
                let result = node.execute(context);
                match result {
                    ContinuationFlow::Return | ContinuationFlow::Break => break 'outer,
                    ContinuationFlow::Continue => break 'inner,
                    ContinuationFlow::Normal => {}
                }
            }
        }

        ContinuationFlow::Normal
    }
}

struct NodeConditional {
    condition: Box<dyn ASTNode>,
    true_block: Vec<Box<dyn ASTNode>>,
    false_block: Vec<Box<dyn ASTNode>>,
}

impl ASTNode for NodeConditional {
    fn execute(&self, context: &mut ExecutionContext) -> ContinuationFlow {
        self.condition.execute(context);
        let condition = context.pop();

        let executing_block = if condition.is_true() {
            &self.true_block
        } else {
            &self.false_block
        };

        for node in executing_block {
            let result = node.execute(context);
            if result != ContinuationFlow::Normal {
                return result;
            }
        }

        ContinuationFlow::Normal
    }
}

struct NodeUnaryOperation {
    op: String,
    rhs: Box<dyn ASTNode>,
}

impl ASTNode for NodeUnaryOperation {
    fn execute(&self, context: &mut ExecutionContext) -> ContinuationFlow {
        self.rhs.execute(context);
        let rhs = context.pop();
        match self.op.as_str() {
            "-" => context.push(-rhs),
            _ => panic!("RuntimeError: invalid unary operand {}", self.op),
        }

        ContinuationFlow::Normal
    }
}

struct NodeBinaryOperation {
    lhs: Box<dyn ASTNode>,
    op: String,
    rhs: Box<dyn ASTNode>,
}

impl ASTNode for NodeBinaryOperation {
    fn execute(&self, context: &mut ExecutionContext) -> ContinuationFlow {
        self.lhs.execute(context);
        self.rhs.execute(context);
        let rhs = context.pop();
        let lhs = context.pop();
        match self.op.as_str() {
            "+" => context.push(lhs + rhs),
            "-" => context.push(lhs - rhs),
            "*" => context.push(lhs * rhs),
            "/" => context.push(lhs / rhs),
            ">" => context.push(Variant::Boolean(lhs > rhs)),
            ">=" => context.push(Variant::Boolean(lhs >= rhs)),
            "<" => context.push(Variant::Boolean(lhs < rhs)),
            "<=" => context.push(Variant::Boolean(lhs <= rhs)),
            "==" => context.push(Variant::Boolean(lhs == rhs)),
            "!=" => context.push(Variant::Boolean(lhs != rhs)),
            _ => panic!("RuntimeError: invalid operand {}", self.op),
        }

        ContinuationFlow::Normal
    }
}

struct NodeVariant(Variant);

impl ASTNode for NodeVariant {
    fn execute(&self, context: &mut ExecutionContext) -> ContinuationFlow {
        match &self.0 {
            Variant::Identifier(ident) => {
                context.push(context.variable_lookup(ident).unwrap().clone())
            }
            _ => context.push(self.0.clone()),
        }

        ContinuationFlow::Normal
    }
}

struct NodeExpressionList(Vec<Box<dyn ASTNode>>);
impl NodeExpressionList {
    fn push(&mut self, value: Box<dyn ASTNode>) {
        self.0.push(value)
    }
}

impl ASTNode for NodeExpressionList {
    fn execute(&self, context: &mut ExecutionContext) -> ContinuationFlow {
        for expr in &self.0 {
            expr.execute(context);
        }

        ContinuationFlow::Normal
    }
}

struct NodeCall {
    func: String,
    args: NodeExpressionList,
    expected_return_count: Option<usize>,
}

impl ASTNode for NodeCall {
    fn execute(&self, context: &mut ExecutionContext) -> ContinuationFlow {
        self.args.execute(context);
        let args = self.args.0.iter().map(|_| Variant::None).collect();

        context.call_native_function(&self.func, args, self.expected_return_count);

        ContinuationFlow::Normal
    }
}

mod helius_std {
    use crate::ExecutionContext;

    pub fn print(context: &mut ExecutionContext) -> usize {
        for arg in context.locals() {
            print!("{}", arg);
        }
        println!();
        0
    }

    pub fn assert(context: &mut ExecutionContext) -> usize {
        let args = context.locals();
        if args.len() != 1 {
            panic!("assert() expects only 1 argument");
        }

        assert!(args[0].is_true());
        0
    }

    pub mod math {
        use crate::{ExecutionContext, Variant};
        use std::convert::TryFrom;

        pub fn pow(context: &mut ExecutionContext) -> usize {
            if context.local_count() != 2 {
                panic!(
                    "pow(n, e) expects 2 arguments, {} given.",
                    context.local_count()
                );
            }

            let args = context.locals();
            let result = match (&args[0], &args[1]) {
                (Variant::Number(a), Variant::Number(b)) => {
                    if b.is_negative() {
                        panic!("pow(n, e) can't have negative exponents until the language supports floating numbers");
                    }

                    i32::pow(*a, u32::try_from(*b).unwrap())
                }
                (_, _) => panic!("pow(n, e) can only be used with numeric arguments."),
            };

            context.push(Variant::Number(result));
            1
        }
    }
}

struct NodeAssignment {
    identifier: NodeVariant,
    expression: Box<dyn ASTNode>,
}

impl ASTNode for NodeAssignment {
    fn execute(&self, context: &mut ExecutionContext) -> ContinuationFlow {
        self.expression.execute(context);
        let value = context.pop();
        if let Variant::Identifier(identifier) = &self.identifier.0 {
            context.variable_set(identifier, value);
        }

        ContinuationFlow::Normal
    }
}

#[derive(Clone)]
struct FunctionInfo {
    stack_base: usize,
    local_variables: Vec<String>,
    arg_count: usize,
}

#[derive(Default)]
pub struct ExecutionContext {
    variables: HashMap<String, Variant>,
    stack: Vec<Variant>,
    functions: Vec<Rc<NodeFunctionBlock>>,
    call_info: Vec<FunctionInfo>,
}

impl ExecutionContext {
    fn push(&mut self, value: Variant) {
        self.stack.push(value);
    }

    fn pop(&mut self) -> Variant {
        self.stack.pop().unwrap()
    }

    fn variable_lookup(&self, name: &str) -> Option<&Variant> {
        if let Some(call_info) = self.call_info.last() {
            if let Some(position) = call_info.local_variables.iter().position(|var| var == name) {
                return Some(&self.stack[call_info.stack_base + position]);
            }
        }
        self.variables.get(name)
    }

    fn local_count(&self) -> usize {
        self.call_info.last().unwrap().arg_count
    }

    fn read_local(&self, index: usize) -> Variant {
        self.stack[self.call_info.last().unwrap().stack_base + index].clone()
    }

    fn locals(&self) -> Vec<Variant> {
        self.stack
            .iter()
            .skip(self.call_info.last().unwrap().stack_base)
            .take(self.call_info.last().unwrap().arg_count)
            .cloned()
            .collect()
    }

    fn variable_set(&mut self, name: &str, value: Variant) {
        self.variables.insert(name.to_string(), value);
    }

    fn add_native_function<F>(&mut self, name: &str, func: &'static F)
    where
        F: Fn(&mut ExecutionContext) -> usize,
    {
        self.variable_set(
            name,
            Variant::NativeFunction(NativeFunction {
                name: name.to_owned(),
                func,
            }),
        );
    }

    fn call_native_function(
        &mut self,
        name: &str,
        args: Vec<Variant>,
        expect_return_count: Option<usize>,
    ) {
        self.call_info.push(FunctionInfo {
            stack_base: self.stack.len() - args.len(),
            local_variables: vec![],
            arg_count: args.len(),
        });

        let return_count;

        match self.variable_lookup(name) {
            Some(Variant::NativeFunction(f)) => {
                return_count = f.clone()(self);
            }
            Some(Variant::Function(block)) => {
                let f = self.functions[(*block) as usize].clone();
                self.call_info.last_mut().unwrap().local_variables = f.arg_names.clone();

                f.execute(self);
                if let Variant::Number(n) = self.pop() {
                    return_count = n as usize;
                } else {
                    panic!("Return count is not a number");
                }
            }
            None => panic!("Trying to call non existent function `{}`", &name),
            _ => {
                panic!("Trying to call variable which is non callable");
            }
        };
        let stack_base = self.call_info.last().unwrap().stack_base;
        let local_count = args.len();
        self.stack.drain(stack_base..stack_base + local_count);

        if let Some(expected_return_count) = expect_return_count {
            let delta = return_count as i32 - expected_return_count as i32;
            match (delta).cmp(&0) {
                std::cmp::Ordering::Less => {
                    for el in iter::repeat_with(|| Variant::None).take(delta.abs() as usize) {
                        self.push(el);
                    }
                }

                std::cmp::Ordering::Greater => {
                    self.stack.drain(..=(self.stack.len() - delta as usize));
                }
                std::cmp::Ordering::Equal => {}
            }
        }

        self.call_info.pop();
    }
}

fn show_usage(program_name: &str, error_msg: &str) {
    println!("usage: {} file_name", program_name);
    println!("error: {}", error_msg)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let program_name = &args[0];
    if args.len() != 2 {
        show_usage(program_name, "file_name is required");
        process::exit(1);
    }

    let file_name = &args[1];
    let file_contents = match fs::read(file_name) {
        Ok(contents) => contents,
        Err(err) => {
            show_usage(program_name, &format!("{}", err));
            process::exit(1);
        }
    };

    let program_source = String::from_utf8_lossy(&file_contents).to_string();

    let tokenizer = Tokenizer::new(program_source);
    let mut parser = Parser::new(tokenizer);

    let parser_time = Instant::now();
    let program = parser.parse();
    let parser_time = parser_time.elapsed().as_secs_f64();
    let mut context = ExecutionContext {
        variables: HashMap::new(),
        stack: vec![],
        functions: parser.functions.into_iter().map(Rc::new).collect(),
        call_info: vec![],
    };

    context.add_native_function("print", &helius_std::print);
    context.add_native_function("assert", &helius_std::assert);
    context.add_native_function("pow", &helius_std::math::pow);

    let execution_time = Instant::now();
    for stmt in program {
        stmt.execute(&mut context);
    }
    if !context.stack.is_empty() {
        println!("Warning: Stack should be empty when program finishes executing, but contains:");
        for (i, v) in context.stack.iter().enumerate() {
            println!("[{}] = {}", i, v);
        }
    }
    let execution_time = execution_time.elapsed().as_secs_f64();

    println!("\n> Parsing took {:.3}ms", parser_time * 1000.0);
    println!("> Execution took {:.3}ms", execution_time * 1000.0);
}
