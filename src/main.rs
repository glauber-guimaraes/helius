#![allow(dead_code)]

use std::{
    cell::RefCell,
    collections::HashMap,
    env, fs,
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
use variant::{MapObject, NativeFunction, Variant};

mod lib;
use lib::helius_std;

mod node;
use node::{ASTNode, NodeFunctionBlock};

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

    fn call_native_function(&mut self, args: usize, expect_return_count: Option<usize>) {
        let function = self.pop();
        self.call_info.push(FunctionInfo {
            stack_base: self.stack.len() - args,
            local_variables: vec![],
            arg_count: args,
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
            Variant::Map(map) => {
                if let Some(metatable) = map.borrow().get_metatable() {
                    if let Some(function) = metatable.borrow().get("__call") {
                        let args_start_index = self.call_info.last().unwrap().stack_base;
                        self.stack
                            .insert(args_start_index, Variant::Map(map.clone()));
                        self.push(function.to_owned());
                        return self.call_native_function(args + 1, expect_return_count);
                    } else {
                        panic!("trying to call object but it's metatable doesn't define a __call method");
                    }
                } else {
                    panic!("trying to call object but it's metatable is empty")
                }
            }
            _ => {
                panic!("Trying to call {:?} which is non callable.", function);
            }
        };
        let stack_base = self.call_info.last().unwrap().stack_base;
        let local_count = args;

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
            (Variant::Map(map), Variant::String(i)) => match map.borrow().get(i) {
                Some(value) => value.clone(),
                None => self.get_metatable_index(map, i).unwrap_or_default(),
            },
            (Variant::Array(array), Variant::Number(i)) => {
                array.borrow().index(*i as usize).clone()
            }
            (Variant::String(s), Variant::Number(i)) => {
                s.chars().nth(*i as usize).unwrap().to_string().into()
            }
            _ => panic!("attempt to index a {:?} with {:?}", object, index),
        };

        self.push(result);
    }

    fn get_metatable_index(&self, map: &RefCell<MapObject>, i: &str) -> Option<Variant> {
        match map.borrow().get_metatable() {
            Some(metatable) => metatable
                .borrow()
                .get(i)
                .cloned()
                .or_else(|| self.get_metatable_index(map, i)),
            None => None,
        }
    }

    fn set_index(&mut self) {
        let value = self.pop();
        let index = self.pop();
        let object = self.pop();

        match (&object, &index) {
            (Variant::Map(map), Variant::String(i)) => {
                let mut map = (**map).borrow_mut();
                map.insert(i.to_owned(), value);
            }
            (Variant::Array(array), Variant::Number(i)) => {
                let mut array = (**array).borrow_mut();
                array[*i as usize] = value;
            }
            _ => panic!("attempt to index a {:?} with {:?}", object, index),
        };
    }

    fn create_array(&mut self, len: usize) {
        let start = self.stack.len() - len;
        let array: Vec<Variant> = self.stack.drain(start..).collect();

        self.push(array.into());
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
    context.add_native_function("range", &helius_std::range);
    context.add_native_function("len", &helius_std::len);
    context.add_native_function("get_metatable", &helius_std::get_metatable);
    context.add_native_function("set_metatable", &helius_std::set_metatable);

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
        .map(|f| (f.name.to_owned(), Variant::NativeFunction(f.clone()))),
    );
    context.variable_set("math", math_module.into());

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
