use std::{cell::RefCell, collections::HashMap, convert::TryInto, iter::FromIterator, rc::Rc};

use crate::{
    variant::{NativeFunction, Variant},
    ExecutionContext,
};

pub trait BaseLibraryLoadExt {
    fn add_base_library(&mut self);
}

impl BaseLibraryLoadExt for ExecutionContext {
    fn add_base_library(&mut self) {
        self.add_native_function("print", &print);
        self.add_native_function("assert", &assert);
        self.add_native_function("range", &range);
        self.add_native_function("len", &len);
        self.add_native_function("get_metatable", &get_metatable);
        self.add_native_function("set_metatable", &set_metatable);
        self.add_native_function("iter", &iter);
    }
}

pub trait MathLibraryLoadExt {
    fn add_math_library(&mut self);
}

impl MathLibraryLoadExt for ExecutionContext {
    fn add_math_library(&mut self) {
        let math_module = HashMap::from_iter(
            [
                NativeFunction {
                    name: "sin".to_owned(),
                    func: &math::sin,
                },
                NativeFunction {
                    name: "cos".to_owned(),
                    func: &math::cos,
                },
                NativeFunction {
                    name: "pow".to_owned(),
                    func: &math::pow,
                },
            ]
            .iter()
            .map(|f| (f.name.to_owned(), Variant::NativeFunction(f.clone()))),
        );
        self.variable_set("math", math_module.into());
    }
}

fn iter(context: &mut ExecutionContext) -> usize {
    let source = context.read_local(0);
    match source {
        Variant::String(_) | Variant::Map(_) | Variant::Array(_) => {
            let mut iterator = HashMap::new();
            iterator.insert("source".to_string(), source);
            iterator.insert("index".to_string(), Variant::Number(0));
            iterator.insert("current".to_string(), Variant::None);
            iterator.insert(
                "next".to_string(),
                Variant::NativeFunction(NativeFunction {
                    name: "iterator_next".to_string(),
                    func: &iterator_next,
                }),
            );

            context.push(iterator.into());
        }
        _ => panic!("TypeError: cannot iterate on {:?}", source),
    }
    1
}

fn iterator_next(context: &mut ExecutionContext) -> usize {
    let this = context.read_local(0);
    let this = match this {
        Variant::Map(m) => m,
        _ => panic!("TypeError: can only call iterator_next on a map object"),
    };

    let mut this = this.borrow_mut();
    let current_index = match this.get("index").unwrap() {
        Variant::Number(i) => *i,
        _ => unreachable!(""),
    };
    let source = this.get("source").unwrap().clone();
    let count = match &source {
        Variant::String(s) => s.len(),
        Variant::Map(m) => m.borrow().len(),
        Variant::Array(a) => a.borrow().len(),
        _ => unreachable!(
            "iterators are created by the compiler, this should always have a compatible type"
        ),
    };
    if current_index == count.try_into().unwrap() {
        context.push(Variant::Boolean(false));
        return 1;
    }

    this.insert(
        "current".to_string(),
        match &source {
            Variant::String(s) => Variant::String(
                s.chars()
                    .nth(current_index.try_into().unwrap())
                    .unwrap()
                    .to_string(),
            ),
            Variant::Map(m) => m
                .borrow()
                .keys()
                .nth(current_index.try_into().unwrap())
                .unwrap()
                .clone()
                .into(),
            Variant::Array(a) => {
                let v = a.borrow();
                v[current_index as usize].clone()
            }
            _ => unreachable!(),
        },
    );

    this.insert("index".to_string(), Variant::Number(current_index + 1));
    context.push(Variant::Boolean(true));
    1
}

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
