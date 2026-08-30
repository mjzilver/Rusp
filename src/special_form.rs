use std::{cell::RefCell, rc::Rc};

use crate::{args::Args, env::Env, eval::eval, parser::Object};

pub fn if_function(args: Vec<Object>, env: &mut Rc<RefCell<Env>>) -> Result<Object, String> {
    let args = Args::new(&args);

    if args.args.len() < 2 || args.args.len() > 3 {
        return Err("Incorrect number of arguments for if".to_string());
    }

    let condition = eval(args.get(0)?.clone(), env)?;

    if is_truthy(&condition) {
        return eval(args.get(1)?.clone(), env);
    }

    match args.args.get(2) {
        Some(else_branch) => eval(else_branch.clone(), env),
        None => Ok(Object::Void()),
    }
}

fn is_truthy(object: &Object) -> bool {
    !matches!(object, Object::Bool(false) | Object::Void())
}

pub fn dotimes_function(args: Vec<Object>, env: &mut Rc<RefCell<Env>>) -> Result<Object, String> {
    let args = Args::new(&args);
    args.exactly(2, "dotimes")?;

    let loop_args = args.list(0, "dotimes first argument should be a list")?;

    if loop_args.len() != 2 {
        return Err("dotimes loop variable and limit expected".to_string());
    }

    let loop_var = match &loop_args[0] {
        Object::Symbol(symbol) => symbol,
        _ => return Err("First item in dotimes must be a symbol".to_string()),
    };

    let limit = match eval(loop_args[1].clone(), env)? {
        Object::Integer(value) => value,
        _ => return Err("Limit in dotimes must evaluate to an integer".to_string()),
    };

    let body = args.get(1)?.clone();

    for i in 0..limit {
        let mut local_env = env.clone();

        local_env
            .borrow_mut()
            .set(loop_var.clone(), Object::Integer(i));

        eval(body.clone(), &mut local_env)?;
    }

    Ok(Object::Void())
}

pub fn cond_function(args: Vec<Object>, env: &mut Rc<RefCell<Env>>) -> Result<Object, String> {
    for clause in args {
        let clause = match clause {
            Object::List(clause) => clause,
            _ => return Err("Invalid cond clause".to_string()),
        };

        match clause.as_slice() {
            [condition, body] => {
                let condition = eval(condition.clone(), env)?;

                if is_truthy(&condition) {
                    return eval(body.clone(), env);
                }
            }

            [body] => {
                // A single-element clause is the default clause.
                return eval(body.clone(), env);
            }

            _ => return Err("Invalid cond clause".to_string()),
        }
    }

    Ok(Object::Bool(false))
}

pub fn defun_function(args: Vec<Object>, env: &mut Rc<RefCell<Env>>) -> Result<Object, String> {
    let args = Args::new(&args);
    args.at_least(3, "defun")?;

    let name = args.symbol(0, "First argument to defun must be a symbol")?;

    let params = args
        .list(1, "Second argument to defun must be a list of parameters")?
        .clone();

    let body = args.args[2..].to_vec();

    env.borrow_mut().set(
        name.to_string(),
        Object::Function {
            name: name.to_string(),
            params,
            body,
        },
    );

    Ok(Object::Void())
}

pub fn let_function(args: Vec<Object>, env: &mut Rc<RefCell<Env>>) -> Result<Object, String> {
    let args = Args::new(&args);
    args.exactly(2, "let")?;

    let bindings = args.list(0, "First argument to let must be a list of bindings")?;

    let body = args.get(1)?.clone();

    let mut local_env = env.clone();

    for binding in bindings {
        let pair = match binding {
            Object::List(pair) => pair,
            _ => return Err("Each binding must be a list".to_string()),
        };

        if pair.len() != 2 {
            return Err("Each binding must be a list of two elements".to_string());
        }

        let var_name = match &pair[0] {
            Object::Symbol(name) => name,
            _ => return Err("Binding variable must be a symbol".to_string()),
        };

        let value = eval(pair[1].clone(), &mut local_env)?;

        local_env.borrow_mut().set(var_name.clone(), value);
    }

    eval(body, &mut local_env)
}

pub fn setq_function(args: Vec<Object>, env: &mut Rc<RefCell<Env>>) -> Result<Object, String> {
    let args = Args::new(&args);
    args.exactly(2, "setq")?;

    let name = args.symbol(0, "First argument to setq must be a symbol")?;

    let value = eval(args.get(1)?.clone(), env)?;

    env.borrow_mut().set(name.to_string(), value.clone());

    Ok(value)
}
