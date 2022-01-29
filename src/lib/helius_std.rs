use std::{cell::RefCell, rc::Rc};

use crate::{variant::Variant, ExecutionContext};

pub fn print(context: &mut ExecutionContext) -> usize {
    for arg in context.locals() {
        print!("{}", arg);
    }
    println!();
    0
}

pub fn len(context: &mut ExecutionContext) -> usize {
    let arg = context.read_local(0);
    let len = match &arg {
        Variant::String(s) => s.chars().count(),
        Variant::Map(map) => map.borrow().len(),
        Variant::Array(array) => array.borrow().len(),
        _ => panic!("Trying to compute len of {:?}", arg),
    } as i32;

    context.push(Variant::Number(len));
    1
}

pub fn assert(context: &mut ExecutionContext) -> usize {
    let args = context.locals();
    if args.len() != 1 {
        panic!("assert() expects 1 argument");
    }

    assert!(args[0].is_true());
    0
}

pub fn range(context: &mut ExecutionContext) -> usize {
    let n = match context.read_local(0) {
        Variant::Number(n) => n,
        _ => panic!("function range expects a numeric argument"),
    };
    context.push(Variant::Array(Rc::new(RefCell::new(
        (0..n).map(Variant::Number).collect(),
    ))));
    1
}

pub fn get_metatable(context: &mut ExecutionContext) -> usize {
    if let Variant::Map(map) = context.read_local(0) {
        context.push(match map.borrow().get_metatable() {
            Some(table) => Variant::Map(table.clone()),
            None => Variant::default(),
        })
    } else {
        panic!("trying to get metatable of non map object");
    }
    1
}

pub fn set_metatable(context: &mut ExecutionContext) -> usize {
    let map = context.read_local(0);
    let table = context.read_local(1);

    match (map, table) {
        (Variant::Map(map), Variant::Map(table)) => {
            map.borrow_mut().metatable = Some(table);
        }
        _ => {
            panic!("trying to set metatable of non map object")
        }
    };

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
