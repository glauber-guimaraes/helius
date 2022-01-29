use std::{
    collections::HashMap,
    fmt::{self, Display},
};

use crate::{variant::Variant, ExecutionContext};

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

pub struct NodeContinue;

impl ASTNode for NodeContinue {
    fn execute(&self, _context: &mut ExecutionContext) -> ContinuationFlow {
        ContinuationFlow::Continue
    }
}

pub struct NodeBreak;

impl ASTNode for NodeBreak {
    fn execute(&self, _context: &mut ExecutionContext) -> ContinuationFlow {
        ContinuationFlow::Break
    }
}

pub struct NodeReturn {
    pub args: NodeExpressionList,
}

impl ASTNode for NodeReturn {
    fn execute(&self, context: &mut ExecutionContext) -> ContinuationFlow {
        self.args.execute(context);
        context.push(Variant::Number(self.args.0.len() as i32));
        ContinuationFlow::Return
    }
}

pub struct NodeFunctionBlock {
    pub arg_names: Vec<String>,
    pub block: Vec<Box<dyn ASTNode>>,
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

pub struct NodeLoop {
    pub condition: Box<dyn ASTNode>,
    pub block: Vec<Box<dyn ASTNode>>,
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

pub struct NodeConditional {
    pub condition: Box<dyn ASTNode>,
    pub true_block: Vec<Box<dyn ASTNode>>,
    pub false_block: Vec<Box<dyn ASTNode>>,
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

pub struct NodeUnaryOperation {
    pub op: String,
    pub rhs: Box<dyn ASTNode>,
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

pub struct NodeBinaryOperation {
    pub lhs: Box<dyn ASTNode>,
    pub op: String,
    pub rhs: Box<dyn ASTNode>,
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

pub struct NodeSetIndex {
    pub base: Box<dyn ASTNode>,
    pub index: Box<dyn ASTNode>,
    pub value: Box<dyn ASTNode>,
}

impl ASTNode for NodeSetIndex {
    fn execute(&self, context: &mut ExecutionContext) -> ContinuationFlow {
        self.base.execute(context);
        self.index.execute(context);
        self.value.execute(context);
        context.set_index();

        ContinuationFlow::Normal
    }
}

pub struct NodeGetIndex {
    pub base: Box<dyn ASTNode>,
    pub name: Box<dyn ASTNode>,
}

impl ASTNode for NodeGetIndex {
    fn execute(&self, context: &mut ExecutionContext) -> ContinuationFlow {
        self.base.execute(context);
        self.name.execute(context);
        context.get_index();

        ContinuationFlow::Normal
    }
}

type RuntimeResult = Result<(), RuntimeError>;

#[derive(Debug)]
pub struct RuntimeError;

impl Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("RuntimeError")
    }
}

impl std::error::Error for RuntimeError {}

pub struct Program {
    instructions: Vec<Box<dyn ASTNode>>,
}

impl Program {
    pub fn new(instructions: Vec<Box<dyn ASTNode>>) -> Self {
        Self { instructions }
    }

    pub fn run(&self, context: &mut ExecutionContext) -> RuntimeResult {
        for instruction in &self.instructions {
            instruction.execute(context);
        }

        Ok(())
    }
}

pub struct NodeVariant(pub Variant);

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

pub struct NodeExpressionList(pub Vec<Box<dyn ASTNode>>);
impl NodeExpressionList {
    pub fn push(&mut self, value: Box<dyn ASTNode>) {
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

pub struct NodeSelfCall {
    pub object: Box<dyn ASTNode>,
    pub func: Box<dyn ASTNode>,
    pub args: NodeExpressionList,
    pub expected_return_count: Option<usize>,
}

impl ASTNode for NodeSelfCall {
    fn execute(&self, context: &mut ExecutionContext) -> ContinuationFlow {
        self.object.execute(context);
        self.args.execute(context);
        self.func.execute(context);

        context.call_self_function(self.args.0.len() + 1, self.expected_return_count);

        ContinuationFlow::Normal
    }
}

pub struct NodeNop {}

impl ASTNode for NodeNop {
    fn execute(&self, _: &mut ExecutionContext) -> ContinuationFlow {
        ContinuationFlow::Normal
    }
}

pub struct NodeCall {
    pub func: Box<dyn ASTNode>,
    pub args: NodeExpressionList,
    pub expected_return_count: Option<usize>,
}

impl ASTNode for NodeCall {
    fn execute(&self, context: &mut ExecutionContext) -> ContinuationFlow {
        self.args.execute(context);
        self.func.execute(context);

        context.call_native_function(self.args.0.len(), self.expected_return_count);

        ContinuationFlow::Normal
    }
}

pub struct NodeAssignment {
    pub identifier: NodeVariant,
    pub expression: Box<dyn ASTNode>,
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

pub struct NodeArrayConstructor {
    pub items: Vec<Box<dyn ASTNode>>,
}

impl ASTNode for NodeArrayConstructor {
    fn execute(&self, context: &mut ExecutionContext) -> ContinuationFlow {
        for item in self.items.iter() {
            item.execute(context);
        }

        context.create_array(self.items.len());

        ContinuationFlow::Normal
    }
}

pub struct NodeMapConstructor {
    pub items: Vec<NodeMapItem>,
}

impl ASTNode for NodeMapConstructor {
    fn execute(&self, context: &mut ExecutionContext) -> ContinuationFlow {
        let map: Variant = HashMap::new().into();
        for item in self.items.iter() {
            context.push(map.clone());
            item.execute(context);
        }

        context.push(map);
        ContinuationFlow::Normal
    }
}

pub struct NodeMapItem {
    pub identifier: String,
    pub expression: Box<dyn ASTNode>,
}

impl ASTNode for NodeMapItem {
    fn execute(&self, context: &mut ExecutionContext) -> ContinuationFlow {
        context.push(self.identifier.to_owned().into());
        self.expression.execute(context);

        context.set_index();
        ContinuationFlow::Normal
    }
}
