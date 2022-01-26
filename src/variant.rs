use std::{
    cmp, fmt,
    ops::{self, Deref},
};

use crate::{
    tokenizer::{Token, TokenType},
    ExecutionContext,
};

#[derive(Debug, Clone)]
pub enum Variant {
    Identifier(String),
    String(String),
    Number(i32),
    Float(f32),
    Boolean(bool),
    NativeFunction(NativeFunction),
    Function(u32),
    None,
}

#[derive(Clone)]
pub struct NativeFunction {
    pub name: String,
    pub func: &'static dyn Fn(&mut ExecutionContext) -> usize,
}

impl std::fmt::Debug for NativeFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&format!("<NativeFunction::{}>", self.name))
    }
}

impl Deref for NativeFunction {
    type Target = &'static dyn Fn(&mut ExecutionContext) -> usize;

    fn deref(&self) -> &Self::Target {
        &self.func
    }
}

impl Variant {
    pub fn is_true(&self) -> bool {
        match self {
            Variant::String(s) => !s.is_empty(),
            Variant::Number(n) => *n != 0,
            Variant::Boolean(b) => *b,
            Variant::None => false,
            _ => true,
        }
    }

    pub fn is_false(&self) -> bool {
        !self.is_true()
    }
}

impl fmt::Display for Variant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Variant::Identifier(ident) => f.write_fmt(format_args!("{}", ident)),
            Variant::String(s) => f.write_fmt(format_args!("{}", s)),
            Variant::Number(n) => f.write_fmt(format_args!("{}", n)),
            Variant::Float(n) => f.write_fmt(format_args!("{}", n)),
            Variant::Boolean(b) => f.write_str(if *b { "True" } else { "False" }),
            Variant::NativeFunction(func) => f.write_fmt(format_args!("{:?}", func)),
            Variant::None => f.write_str("None"),
            Variant::Function(_) => f.write_str("<Function>"),
        }
    }
}

impl From<Token> for Variant {
    fn from(token: Token) -> Self {
        match token.r#type {
            TokenType::Identifier => Self::Identifier(token.lexeme),
            TokenType::Number => Self::Number(token.lexeme.parse().unwrap()),
            TokenType::Float => Self::Float(token.lexeme.parse().unwrap()),
            TokenType::String => Self::String(token.lexeme),
            TokenType::Boolean => Self::Boolean(token.lexeme == "true"),
            TokenType::None => Self::None,
            _ => panic!("Invalid conversion for token {:?}", token),
        }
    }
}

impl cmp::PartialEq for Variant {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            (Self::Number(l0), Self::Number(r0)) => l0 == r0,
            (Self::Float(n0), Self::Float(n1)) => n0 == n1,
            (Self::Number(n0), Self::Float(n1)) => *n0 as f32 == *n1,
            (Self::Float(n0), Self::Number(n1)) => *n0 == *n1 as f32,
            (Self::None, Self::None) => true,
            (_, Self::None) => false,
            (Self::None, _) => false,
            (Self::String(_), _) => false,
            (_, Self::String(_)) => false,
            (a, b) => a.is_true() && b.is_true(),
        }
    }
}

impl cmp::PartialOrd for Variant {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        match (self, other) {
            (Variant::Number(n1), Variant::Number(n2)) => n1.partial_cmp(n2),
            (Self::Float(n1), Self::Float(n2)) => n1.partial_cmp(n2),
            (Self::Number(n1), Self::Float(n2)) => (*n1 as f32).partial_cmp(n2),
            (Self::Float(n1), Self::Number(n2)) => n1.partial_cmp(&(*n2 as f32)),
            _ => panic!("RuntimeError: cannot compare {:?} and {:?}", self, other),
        }
    }
}

impl ops::Add for Variant {
    type Output = Variant;

    fn add(self, rhs: Self) -> Self::Output {
        match (&self, &rhs) {
            (Variant::Number(lhs), Variant::Number(rhs)) => Variant::Number(*lhs + *rhs),
            (Self::Float(lhs), Self::Float(rhs)) => Variant::Float(*lhs + *rhs),
            (Self::Number(lhs), Self::Float(rhs)) => Variant::Float(*lhs as f32 + *rhs),
            (Self::Float(lhs), Self::Number(rhs)) => Variant::Float(*lhs + *rhs as f32),
            _ => {
                panic!("RuntimeError: cannot add {:?} and {:?}", self, rhs);
            }
        }
    }
}

impl ops::Sub for Variant {
    type Output = Variant;

    fn sub(self, rhs: Self) -> Self::Output {
        match (&self, &rhs) {
            (Variant::Number(lhs), Variant::Number(rhs)) => Variant::Number(*lhs - *rhs),
            (Self::Float(lhs), Self::Float(rhs)) => Variant::Float(*lhs - *rhs),
            (Self::Number(lhs), Self::Float(rhs)) => Variant::Float(*lhs as f32 - *rhs),
            (Self::Float(lhs), Self::Number(rhs)) => Variant::Float(*lhs - *rhs as f32),
            _ => {
                panic!("RuntimeError: cannot add {:?} and {:?}", self, rhs);
            }
        }
    }
}

impl ops::Mul for Variant {
    type Output = Variant;

    fn mul(self, rhs: Self) -> Self::Output {
        match (&self, &rhs) {
            (Variant::Number(lhs), Variant::Number(rhs)) => Variant::Number(*lhs * *rhs),
            (Self::Float(lhs), Self::Float(rhs)) => Variant::Float(*lhs * *rhs),
            (Self::Number(lhs), Self::Float(rhs)) => Variant::Float(*lhs as f32 * *rhs),
            (Self::Float(lhs), Self::Number(rhs)) => Variant::Float(*lhs * *rhs as f32),
            _ => {
                panic!("RuntimeError: cannot add {:?} and {:?}", self, rhs);
            }
        }
    }
}

impl ops::Div for Variant {
    type Output = Variant;

    fn div(self, rhs: Self) -> Self::Output {
        match (&self, &rhs) {
            (Variant::Number(lhs), Variant::Number(rhs)) => Variant::Number(*lhs / *rhs),
            (Self::Float(lhs), Self::Float(rhs)) => Variant::Float(*lhs / *rhs),
            (Self::Number(lhs), Self::Float(rhs)) => Variant::Float(*lhs as f32 / *rhs),
            (Self::Float(lhs), Self::Number(rhs)) => Variant::Float(*lhs / *rhs as f32),
            _ => {
                panic!("RuntimeError: cannot add {:?} and {:?}", self, rhs);
            }
        }
    }
}

impl ops::Neg for Variant {
    type Output = Variant;

    fn neg(self) -> Self::Output {
        match self {
            Variant::Number(n) => Variant::Number(-n),
            Self::Float(n) => Variant::Float(-n),
            _ => {
                panic!(
                    "RuntimeError: cannot apply unary operator `-` to {:?}",
                    self
                );
            }
        }
    }
}
