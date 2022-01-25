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
            _ => panic!("RuntimeError: cannot compare {:?} and {:?}", self, other),
        }
    }
}

impl ops::Add for Variant {
    type Output = Variant;

    fn add(self, rhs: Self) -> Self::Output {
        if let (Variant::Number(lhs), Variant::Number(rhs)) = (&self, &rhs) {
            Variant::Number(*lhs + *rhs)
        } else {
            panic!("RuntimeError: cannot add {:?} and {:?}", self, rhs);
        }
    }
}

impl ops::Sub for Variant {
    type Output = Variant;

    fn sub(self, rhs: Self) -> Self::Output {
        if let (Variant::Number(lhs), Variant::Number(rhs)) = (&self, &rhs) {
            Variant::Number(*lhs - *rhs)
        } else {
            panic!("RuntimeError: cannot add {:?} and {:?}", self, rhs);
        }
    }
}

impl ops::Mul for Variant {
    type Output = Variant;

    fn mul(self, rhs: Self) -> Self::Output {
        if let (Variant::Number(lhs), Variant::Number(rhs)) = (&self, &rhs) {
            Variant::Number(*lhs * *rhs)
        } else {
            panic!("RuntimeError: cannot add {:?} and {:?}", self, rhs);
        }
    }
}

impl ops::Div for Variant {
    type Output = Variant;

    fn div(self, rhs: Self) -> Self::Output {
        if let (Variant::Number(lhs), Variant::Number(rhs)) = (&self, &rhs) {
            Variant::Number(*lhs / *rhs)
        } else {
            panic!("RuntimeError: cannot add {:?} and {:?}", self, rhs);
        }
    }
}

impl ops::Neg for Variant {
    type Output = Variant;

    fn neg(self) -> Self::Output {
        if let Variant::Number(n) = self {
            Variant::Number(-n)
        } else {
            panic!(
                "RuntimeError: cannot apply unary operator `-` to {:?}",
                self
            );
        }
    }
}
