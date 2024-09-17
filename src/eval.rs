use std::cell::RefCell;
use std::rc::Rc;

use crate::builtins::{self, get_builtin_function};
use crate::{env::Env, parser::Object};

pub fn eval(object: Object, env: &mut Rc<RefCell<Env>>) -> Result<Object, String> {
    match object {
        Object::Integer(_) => Ok(object),
        Object::String(_) => Ok(object),
        Object::Bool(_) => Ok(object),
        Object::Symbol(ref s) => eval_symbol(s, env),
        Object::List(ref list) => eval_list(list, env),
        Object::Void() => Ok(Object::Void()),

        // Functions do not get eval
        Object::Function { .. } => Err("Unexpected function object in this context".to_string()),
        Object::Stack(_) => Err("Unexpected Stack object in this context".to_string()),
    }
}

pub fn eval_stack(stack_object: Object, env: &mut Rc<RefCell<Env>>) -> Result<String, String> {
    let mut output = String::new();
    if let Object::Stack(ref stack) = stack_object {
        for object in stack {
            match &eval(object.clone(), env) {
                Ok(eval_obj) => {
                    if std::env::var("DEBUG_MODE").is_ok() {
                        output += &eval_obj.to_string();
                    }
                }
                Err(err) => output += err,
            }
        }

        return Ok(output);
    }

    Err("Eval stack requires a stack object".to_string())
}

fn eval_symbol(s: &String, env: &mut Rc<RefCell<Env>>) -> Result<Object, String> {
    if let Some(_) = get_builtin_function(s) {
        return Ok(Object::Symbol(s.clone()));
    }

    env.borrow_mut()
        .get(s)
        .ok_or_else(|| format!("Undefined symbol: {}", s))
}

fn eval_list(list: &Vec<Object>, env: &mut Rc<RefCell<Env>>) -> Result<Object, String> {
    if list.is_empty() {
        return Err("Empty list".to_string());
    }

    let func = eval(list[0].clone(), env)?;
    let args = &list[1..];

    // Check for special forms
    if let Object::Symbol(ref s) = func {
        match s.as_str() {
            "let" => return builtins::let_function(args.to_vec(), env),
            "defun" => return builtins::defun_function(args.to_vec(), env),
            "dotimes" => return builtins::dotimes_function(args.to_vec(), env),
            "cond" => return builtins::cond_function(args.to_vec(), env),
            "setq" => return builtins::setq_function(args.to_vec(), env),
            "if" => return builtins::if_function(args.to_vec(), env),
            _ => {}
        }
    }

    apply_function(func, args.to_vec(), env)
}

fn apply_function(
    func: Object,
    args: Vec<Object>,
    env: &mut Rc<RefCell<Env>>,
) -> Result<Object, String> {
    let evaluated_args: Result<Vec<Object>, String> =
        args.into_iter().map(|arg| eval(arg, env)).collect();
    let evaluated_args = evaluated_args?;

    match func {
        Object::Symbol(ref s) => {
            if let Some(built_in) = get_builtin_function(s.as_str()) {
                built_in(evaluated_args, env)
            } else {
                Err(format!("Unknown function: {}", s))
            }
        }
        Object::Function { name, params, body } => {
            if params.len() != evaluated_args.len() {
                return Err(format!(
                    "Incorrect number of arguments for function: {}",
                    name
                ));
            }

            let mut local_env = env.clone();
            for (param, arg) in params.into_iter().zip(evaluated_args) {
                local_env.borrow_mut().set(param.to_string(), arg);
            }

            let mut last_result = Object::Void();
            for obj in body {
                last_result = eval(obj, &mut local_env)?;
            }

            Ok(last_result)
        }
        _ => Err("Function application on non-function".to_string()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::env::Env;
    use crate::parser::Object;

    #[test]
    fn test_eval_addition() {
        // Arrange
        let mut env = Rc::new(RefCell::new(Env::new()));
        let input = Object::List(vec![
            Object::Symbol("+".to_string()),
            Object::Integer(1),
            Object::Integer(2),
        ]);

        // Act
        let result = eval(input, &mut env);

        // Assert
        let expected = Ok(Object::Integer(3));
        assert_eq!(result, expected);
    }
}
