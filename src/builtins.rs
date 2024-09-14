use crate::{env::Env, eval::eval, parser::Object};

pub type BuiltInFunction = fn(Vec<Object>, env: &mut Env) -> Result<Object, String>;

pub fn get_builtin_function(name: &str) -> Option<BuiltInFunction> {
    match name {
        "+" => Some(add_function),
        "-" => Some(minus_function),
        "define" => Some(define_function),
        _ => None,
    }
}

fn add_function(args: Vec<Object>, _env: &mut Env) -> Result<Object, String> {
    let mut sum = 0;

    for arg in args {
        match arg {
            Object::Integer(n) => sum += n,
            _ => return Err("Cannot use + with non-integer values".to_string()),
        }
    }

    Ok(Object::Integer(sum))
}

fn minus_function(args: Vec<Object>, _env: &mut Env) -> Result<Object, String> {
    let mut result = 0;

    for arg in args {
        match arg {
            Object::Integer(n) => result -= n,
            _ => return Err("Cannot use + with non-integer values".to_string()),
        }
    }

    Ok(Object::Integer(result))
}

fn define_function(args: Vec<Object>, env: &mut Env) -> Result<Object, String> {
    if args.len() != 2 {
        return Err("Incorrect number of arguments for define".to_string());
    }

    if let Object::Symbol(ref name) = args[0] {
        let value = eval(args[1].clone(), env)?;
        env.set(name.clone(), value.clone());
        Ok(value)
    } else {
        Err("First argument to define must be a symbol".to_string())
    }
}
