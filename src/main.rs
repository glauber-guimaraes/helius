#![allow(dead_code)]

use std::{
    cell::RefCell,
    collections::HashMap,
    convert::TryInto,
    env,
    error::Error,
    fs,
    iter::{self},
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

mod node;
use node::{ASTNode, NodeFunctionBlock};

use crate::lib::helius_std::{BaseLibraryLoadExt, MathLibraryLoadExt};

#[derive(Clone)]
struct FunctionInfo {
    stack_base: usize,
    arg_count: usize,
    local_count: usize,
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
        self.variables.get(name)
    }

    fn local_count(&self) -> usize {
        self.call_info.last().unwrap().arg_count
    }

    fn read_local(&self, index: usize) -> Variant {
        self.stack[self.call_info.last().unwrap().stack_base + index].clone()
    }

    fn write_local(&mut self, index: usize, value: &Variant) {
        self.stack[self.call_info.last().unwrap().stack_base + index] = value.clone();
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

    fn print_stack(&self) {
        println!("> Stack:");
        for (i, var) in self.stack.iter().enumerate() {
            println!("[{}] {:?}", i, var);
        }
        println!();
    }

    fn call_self_function(&mut self, args: usize, expect_return_count: Option<usize>) {
        let index = self.pop();
        let object = self.stack[self.stack.len() - args].clone();

        self.push(object);
        self.push(index);
        self.get_index();

        self.call_native_function(args, expect_return_count);
    }

    fn call_native_function(&mut self, args: usize, expect_return_count: Option<usize>) {
        let function = self.pop();
        self.call_info.push(FunctionInfo {
            stack_base: self.stack.len() - args,
            arg_count: args,
            local_count: args,
        });

        let return_count;

        match function {
            Variant::NativeFunction(f) => {
                return_count = f(self);
            }
            Variant::Function(block) => {
                let f = self.functions[block as usize].clone();

                self.call_info.last_mut().unwrap().arg_count = f.arg_count;
                self.call_info.last_mut().unwrap().local_count = f.locals_count;

                if args < f.arg_count {
                    self.stack
                        .extend(iter::repeat(Variant::None).take(f.arg_count - args))
                }

                self.stack
                    .extend(iter::repeat(Variant::None).take(f.locals_count - f.arg_count));

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
        let local_count = self.call_info.last().unwrap().local_count;

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

    fn reserve_locals(&mut self, entry_locals: usize) {
        self.call_info.push(FunctionInfo {
            stack_base: 0,
            arg_count: 0,
            local_count: entry_locals,
        });
        self.stack
            .extend(iter::repeat(Variant::None).take(entry_locals));
    }

    fn finish(&mut self) {
        let stack_base = self.call_info.last().unwrap().stack_base;
        let local_count = self.call_info.last().unwrap().local_count;

        self.stack.drain(stack_base..stack_base + local_count);
    }
}

fn show_usage(program_name: &str, error_msg: &str) {
    println!("usage: {} file_name", program_name);
    println!("error: {}", error_msg)
}

fn main() -> Result<(), Box<dyn Error>> {
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
    let program = parser.parse()?;
    let parser_time = parser_time.elapsed().as_secs_f64();
    let mut context = ExecutionContext {
        variables: HashMap::new(),
        stack: vec![],
        functions: parser.functions.into_iter().map(Rc::new).collect(),
        call_info: vec![],
    };

    context.add_base_library();
    context.add_math_library();

    let execution_time = Instant::now();
    program.run(&mut context)?;

    if context.stack.is_empty() {
        println!("Program finished. No return values");
    } else if let Variant::Number(return_count) = context.pop() {
        assert_eq!(
            context.stack.len(),
            return_count.try_into().unwrap(),
            "Stack didn't have all returned variables"
        );
        println!("Program finished. Return values:");
        print!("{}", context.stack[0]);
        for v in context.stack.iter().skip(1) {
            print!(", {}", v);
        }
        println!();
    } else {
        eprintln!("Error: Stack should contain return values when program finishes executing, but top doesn't have a return count:");
        for (i, v) in context.stack.iter().enumerate() {
            eprintln!("[{}] = {}", i, v);
        }
    }

    let execution_time = execution_time.elapsed().as_secs_f64();

    println!("\n> Parsing took {:.3}ms", parser_time * 1000.0);
    println!("> Execution took {:.3}ms", execution_time * 1000.0);

    Ok(())
}

#[cfg(test)]
mod tests {

    use crate::lib::helius_std::{BaseLibraryLoadExt, MathLibraryLoadExt};

    use super::*;

    type TestResult = Result<(), Box<dyn Error>>;

    #[test]
    fn test_function_definition_statement() -> TestResult {
        can_run_program(
            r#"
            function foo()
                return 5
            end

            assert(foo() == 5)
        "#,
        )
    }

    fn can_run_program(program: &str) -> TestResult {
        let tokenizer = Tokenizer::new(program.to_string());
        let mut parser = Parser::new(tokenizer);
        let program = parser.parse()?;

        let mut context = ExecutionContext {
            variables: HashMap::new(),
            stack: vec![],
            functions: parser.functions.into_iter().map(Rc::new).collect(),
            call_info: vec![],
        };

        context.add_base_library();
        context.add_math_library();

        program.run(&mut context)?;

        Ok(())
    }
}
