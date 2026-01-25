use std::{cell::RefCell, rc::Rc};

use crate::{env::Env, eval::eval, parser::Object};

pub fn if_function(args: Vec<Object>, env: &mut Rc<RefCell<Env>>) -> Result<Object, String> {
    if args.len() < 2 || args.len() > 3 {
        return Err("Incorrect number of arguments for if".to_string());
    }

    let condition = eval(args[0].clone(), env)?;

    let is_truthy = match condition {
        Object::Bool(false) => false,
        Object::Void() => false,
        _ => true,
    };

    if is_truthy {
        return Ok(eval(args[1].clone(), env)?);
    } else if args.len() == 3 {
        return Ok(eval(args[2].clone(), env)?);
    }

    Ok(Object::Void())
}

pub fn dotimes_function(args: Vec<Object>, env: &mut Rc<RefCell<Env>>) -> Result<Object, String> {
    if args.len() != 2 {
        return Err("dotimes expects 2 arguments".to_string());
    }

    let loop_args = match &args[0] {
        Object::List(list) => list,
        _ => return Err("dotimes first argument should be a list".to_string()),
    };

    if loop_args.len() != 2 {
        return Err("dotimes loop variable and limit expected".to_string());
    }

    let loop_var = match &loop_args[0] {
        Object::Symbol(s) => s,
        _ => return Err("First item in dotimes must be a symbol".to_string()),
    };

    let limit = match &eval(loop_args[1].clone(), env)? {
        Object::Integer(i) => *i,
        _ => return Err("Limit in dotimes must evaluate to an integer".to_string()),
    };

    let body = &args[1];

    for i in 0..limit {
        let mut local_env = env.clone();
        local_env
            .borrow_mut()
            .set(loop_var.clone(), Object::Integer(i));

        let result = eval(body.clone(), &mut local_env);
        if result.is_err() {
            return result;
        }
    }

    Ok(Object::Void())
}

pub fn cond_function(args: Vec<Object>, env: &mut Rc<RefCell<Env>>) -> Result<Object, String> {
    for clause in &args {
        match clause {
            Object::List(pair) if pair.len() == 2 => {
                let condition = eval(pair[0].clone(), env)?;

                if condition == Object::Bool(true) {
                    return eval(pair[1].clone(), env);
                }
            }
            Object::List(pair) if pair.len() == 1 => {
                return eval(pair[0].clone(), env);
            }
            _ => return Err("Invalid cond clause".to_string()),
        }
    }

    Ok(Object::Bool(false))
}

pub fn defun_function(args: Vec<Object>, env: &mut Rc<RefCell<Env>>) -> Result<Object, String> {
    if args.len() < 3 {
        return Err(format!(
            "Incorrect number of arguments for defun want at least 3 got={}",
            args.len()
        ));
    }

    if let Object::Symbol(ref name) = &args[0] {
        let params = match &args[1] {
            Object::List(list) => list.clone(),
            _ => return Err("Second argument to defun must be a list of parameters".to_string()),
        };

        let body: Vec<Object> = args[2..].to_vec();

        env.borrow_mut().set(
            name.clone(),
            Object::Function {
                name: name.to_string(),
                params,
                body,
            },
        );

        Ok(Object::Void())
    } else {
        Err("First argument to defun must be a symbol".to_string())
    }
}

pub fn let_function(args: Vec<Object>, env: &mut Rc<RefCell<Env>>) -> Result<Object, String> {
    if args.len() != 2 {
        return Err("Incorrect number of arguments for let".to_string());
    }

    let bindings = &args[0];
    let body = &args[1];

    if let Object::List(ref bindings_list) = bindings {
        let mut local_env = env.clone();

        for binding in bindings_list {
            if let Object::List(ref pair) = binding {
                if pair.len() != 2 {
                    return Err("Each binding must be a list of two elements".to_string());
                }

                let var = &pair[0];
                let value = eval(pair[1].clone(), &mut local_env)?;
                if let Object::Symbol(ref var_name) = var {
                    local_env.borrow_mut().set(var_name.clone(), value);
                } else {
                    return Err("Binding variable must be a symbol".to_string());
                }
            } else {
                return Err("Each binding must be a list".to_string());
            }
        }

        eval(body.clone(), &mut local_env)
    } else {
        Err("First argument to let must be a list of bindings".to_string())
    }
}

pub fn setq_function(args: Vec<Object>, env: &mut Rc<RefCell<Env>>) -> Result<Object, String> {
    if args.len() != 2 {
        return Err("Incorrect number of arguments for setq".to_string());
    }

    if let Object::Symbol(ref name) = args[0] {
        let value = eval(args[1].clone(), env)?;
        env.borrow_mut().set(name.clone(), value.clone());
        Ok(value)
    } else {
        Err("First argument to let must be a symbol".to_string())
    }
}
