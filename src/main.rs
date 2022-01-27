#![allow(dead_code)]

use std::{
    collections::HashMap,
    env::{self},
    fmt, fs,
    iter::{self, FromIterator},
    ops::Index,
    process,
    rc::Rc,
    time::Instant,
};

mod tokenizer;
use tokenizer::Tokenizer;

mod parser;
use parser::Parser;

mod variant;
use variant::{NativeFunction, Variant};

#[derive(PartialEq, Debug)]
pub enum ContinuationFlow {
    Normal,
    Return,
    Break,
    Continue,
}

pub trait ASTNode {
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

pub struct NodeFunctionBlock {
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

struct NodeGetIndex {
    base: Box<dyn ASTNode>,
    name: String,
}

impl ASTNode for NodeGetIndex {
    fn execute(&self, context: &mut ExecutionContext) -> ContinuationFlow {
        self.base.execute(context);
        context.push(Variant::String(self.name.clone()));
        context.get_index();

        ContinuationFlow::Normal
    }
}

struct NodeVariant(Variant);

impl ASTNode for NodeVariant {
    fn execute(&self, context: &mut ExecutionContext) -> ContinuationFlow {
        match &self.0 {
            Variant::Identifier(ident) => context.push(
                context
                    .variable_lookup(ident)
                    .unwrap_or(&Variant::None)
                    .clone(),
            ),
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
    func: Box<dyn ASTNode>,
    args: NodeExpressionList,
    expected_return_count: Option<usize>,
}

impl ASTNode for NodeCall {
    fn execute(&self, context: &mut ExecutionContext) -> ContinuationFlow {
        self.args.execute(context);
        let args = self.args.0.iter().map(|_| Variant::None).collect();
        self.func.execute(context);

        context.call_native_function(args, self.expected_return_count);

        ContinuationFlow::Normal
    }
}

mod helius_std {
    use std::{collections::HashMap, rc::Rc};

    use crate::{variant::Variant, ExecutionContext};

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
            panic!("assert() expects 1 argument");
        }

        assert!(args[0].is_true());
        0
    }

    pub fn map(context: &mut ExecutionContext) -> usize {
        context.push(Variant::Map(Rc::new(HashMap::new())));
        1
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

        pub fn sin(context: &mut ExecutionContext) -> usize {
            let arg = context.read_local(0);
            let result = Variant::Float(match &arg {
                Variant::Number(n) => f32::sin(*n as f32),
                Variant::Float(f) => f32::sin(*f),
                _ => panic!(
                    "function math.sin expects a numeric argument, got {:?}",
                    arg
                ),
            });
            context.push(result);
            1
        }

        pub fn cos(context: &mut ExecutionContext) -> usize {
            let arg = context.read_local(0);
            let result = Variant::Float(match &arg {
                Variant::Number(n) => f32::cos(*n as f32),
                Variant::Float(f) => f32::cos(*f),
                _ => panic!(
                    "function math.sin expects a numeric argument, got {:?}",
                    arg
                ),
            });
            context.push(result);
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

    fn call_native_function(&mut self, args: Vec<Variant>, expect_return_count: Option<usize>) {
        let function = self.pop();
        self.call_info.push(FunctionInfo {
            stack_base: self.stack.len() - args.len(),
            local_variables: vec![],
            arg_count: args.len(),
        });

        let return_count;

        match function {
            Variant::NativeFunction(f) => {
                return_count = f(self);
            }
            Variant::Function(block) => {
                let f = self.functions[block as usize].clone();
                self.call_info.last_mut().unwrap().local_variables = f.arg_names.clone();

                f.execute(self);
                if let Variant::Number(n) = self.pop() {
                    return_count = n as usize;
                } else {
                    panic!("Return count is not a number");
                }
            }
            _ => {
                panic!("Trying to call {:?} which is non callable.", function);
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

    fn get_index(&mut self) {
        let index = self.pop();
        let object = self.pop();

        let result = match (&object, &index) {
            (Variant::Map(map), Variant::String(i)) => map.get(i).unwrap_or(&Variant::None).clone(),
            (Variant::Array(array), Variant::Number(i)) => array.index(*i as usize).clone(),
            _ => panic!("attempt to index a {:?} with {:?}", object, index),
        };

        self.push(result);
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
    context.add_native_function("map", &helius_std::map);

    let math_module = HashMap::from_iter(
        [
            NativeFunction {
                name: "sin".to_owned(),
                func: &helius_std::math::sin,
            },
            NativeFunction {
                name: "cos".to_owned(),
                func: &helius_std::math::cos,
            },
        ]
        .iter()
        .map(|f| (f.name.clone(), Variant::NativeFunction(f.clone()))),
    );
    context.variable_set("math", Variant::Map(Rc::new(math_module)));

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
