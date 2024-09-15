use crate::{env::Env, eval::eval, parser::Object};

pub type BuiltInFunction = fn(Vec<Object>, env: &mut Env) -> Result<Object, String>;

pub fn get_builtin_function(name: &str) -> Option<BuiltInFunction> {
    match name {
        // Arithmetic
        "+" => Some(add_function),
        "-" => Some(minus_function),
        "*" => Some(multiply_function),
        "/" => Some(divide_function),
        "mod" => Some(mod_function),

        // Comparison
        "not" => Some(not_function),
        "=" => Some(|args, _| compare_objects(args, equals)),
        "/=" => Some(not_equals_all),
        ">" => Some(|args, _| compare_objects(args, greater_than)),
        "<" => Some(|args, _| compare_objects(args, lesser_than)),
        ">=" => Some(|args, _| compare_objects(args, greater_than_or_equals)),
        "<=" => Some(|args, _| compare_objects(args, lesser_than_or_equals)),
        "zerop" => Some(zerop_function),
        "and" => Some(and_function),

        // Variables
        "let" => Some(let_function),
        "defun" => Some(defun_function),

        // control flow
        "if" => Some(if_function),
        "dotimes" => Some(dotimes_function),
        "cond" => Some(cond_function),

        // IO
        "print" => Some(print_function),
        _ => None,
    }
}

fn add_function(args: Vec<Object>, _env: &mut Env) -> Result<Object, String> {
    if let Some(Object::Integer(_)) = args.get(0) {
        let mut sum = 0;

        for arg in args {
            match arg {
                Object::Integer(n) => sum += n,
                _ => return Err(format!("Cannot add {} to integer", arg)),
            }
        }

        Ok(Object::Integer(sum))
    } else if let Some(Object::String(_)) = args.get(0) {
        let mut string = String::new();

        for arg in args {
            match arg {
                Object::String(s) => string += &s.to_string(),
                _ => return Err(format!("Cannot add {} to string", arg)),
            }
        }

        Ok(Object::String(string))
    } else {
        return Err(format!("Cannot add to {}", args.get(0).unwrap()));
    }
}

fn minus_function(args: Vec<Object>, _env: &mut Env) -> Result<Object, String> {
    if let Some(Object::Integer(first_value)) = args.get(0) {
        let mut result = *first_value;

        for arg in &args[1..] {
            match arg {
                Object::Integer(n) => result -= n,
                _ => return Err("Cannot use - with non-integer values".to_string()),
            }
        }

        Ok(Object::Integer(result))
    } else {
        Err("First argument must be an integer".to_string())
    }
}

fn multiply_function(args: Vec<Object>, _env: &mut Env) -> Result<Object, String> {
    if let Some(Object::Integer(first_value)) = args.get(0) {
        let mut result = *first_value;

        for arg in &args[1..] {
            match arg {
                Object::Integer(n) => result *= n,
                _ => return Err("Cannot use * with non-integer values".to_string()),
            }
        }

        Ok(Object::Integer(result))
    } else {
        Err("First argument must be an integer".to_string())
    }
}

fn divide_function(args: Vec<Object>, _env: &mut Env) -> Result<Object, String> {
    if let Some(Object::Integer(first_value)) = args.get(0) {
        let mut result = *first_value;

        for arg in &args[1..] {
            match arg {
                Object::Integer(n) => result /= n,
                _ => return Err("Cannot use / with non-integer values".to_string()),
            }
        }

        Ok(Object::Integer(result))
    } else {
        Err("First argument must be an integer".to_string())
    }
}

fn mod_function(args: Vec<Object>, _env: &mut Env) -> Result<Object, String> {
    if args.len() != 2 {
        return Err("Incorrect number of arguments for mod".to_string());
    }

    let a = match &args[0] {
        Object::Integer(n) => *n,
        _ => return Err("First argument to mod must be an integer".to_string()),
    };

    let b = match &args[1] {
        Object::Integer(n) => *n,
        _ => return Err("Second argument to mod must be an integer".to_string()),
    };

    if b == 0 {
        return Err("Division by zero in mod".to_string());
    }

    Ok(Object::Integer(a % b))
}

fn print_function(args: Vec<Object>, _env: &mut Env) -> Result<Object, String> {
    if args.is_empty() {
        return Err("No args given to print".to_string());
    }

    for arg in args {
        match arg {
            Object::Integer(n) => println!("{}", n),
            Object::String(s) => println!("{}", s),
            Object::Bool(b) => println!("{}", b),
            _ => return Err("Cannot print this type".to_string()),
        }
    }

    Ok(Object::Void())
}

fn not_function(args: Vec<Object>, _env: &mut Env) -> Result<Object, String> {
    if args.len() != 1 {
        return Err("Incorrect number of arguments for let".to_string());
    }

    if let Object::Bool(ref bool) = args[0] {
        Ok(Object::Bool(!bool))
    } else {
        Err("Argument to not must be a bool".to_string())
    }
}

type CompareFn = fn(&Object, &Object) -> bool;

fn compare_objects(args: Vec<Object>, comparison: CompareFn) -> Result<Object, String> {
    if args.len() < 2 {
        return Err("Require at least 2 items to compare".to_string());
    }

    for i in 0..(args.len() - 1) {
        let item1 = &args[i];
        let item2 = &args[i + 1];

        match (item1, item2) {
            (Object::Bool(bool1), Object::Bool(bool2)) => {
                if !comparison(&Object::Bool(*bool1), &Object::Bool(*bool2)) {
                    return Ok(Object::Bool(false));
                }
            }
            (Object::Integer(int1), Object::Integer(int2)) => {
                if !comparison(&Object::Integer(*int1), &Object::Integer(*int2)) {
                    return Ok(Object::Bool(false));
                }
            }
            _ => {
                return Err("Unsupported comparison between different types".to_string());
            }
        }
    }

    Ok(Object::Bool(true))
}

fn greater_than(item1: &Object, item2: &Object) -> bool {
    match (item1, item2) {
        (Object::Bool(bool1), Object::Bool(bool2)) => bool1 > bool2,
        (Object::Integer(int1), Object::Integer(int2)) => int1 > int2,
        _ => false,
    }
}

fn lesser_than(item1: &Object, item2: &Object) -> bool {
    match (item1, item2) {
        (Object::Bool(bool1), Object::Bool(bool2)) => bool1 < bool2,
        (Object::Integer(int1), Object::Integer(int2)) => int1 < int2,
        _ => false,
    }
}

fn greater_than_or_equals(item1: &Object, item2: &Object) -> bool {
    match (item1, item2) {
        (Object::Bool(bool1), Object::Bool(bool2)) => bool1 >= bool2,
        (Object::Integer(int1), Object::Integer(int2)) => int1 >= int2,
        _ => false,
    }
}

fn lesser_than_or_equals(item1: &Object, item2: &Object) -> bool {
    match (item1, item2) {
        (Object::Bool(bool1), Object::Bool(bool2)) => bool1 <= bool2,
        (Object::Integer(int1), Object::Integer(int2)) => int1 <= int2,
        _ => false,
    }
}

fn equals(item1: &Object, item2: &Object) -> bool {
    match (item1, item2) {
        (Object::Bool(bool1), Object::Bool(bool2)) => bool1 == bool2,
        (Object::Integer(int1), Object::Integer(int2)) => int1 == int2,
        (Object::String(str1), Object::String(str2)) => str1 == str2,
        _ => false,
    }
}

fn not_equals_all(args: Vec<Object>, _env: &mut Env) -> Result<Object, String> {
    if args.len() < 2 {
        return Err("Require at least 2 items to compare".to_string());
    }

    for i in 0..args.len() {
        for j in (i + 1)..args.len() {
            if equals(&args[i], &args[j]) {
                return Ok(Object::Bool(false));
            }
        }
    }

    Ok(Object::Bool(true))
}

fn zerop_function(args: Vec<Object>, _env: &mut Env) -> Result<Object, String> {
    if args.len() != 1 {
        return Err("Incorrect number of arguments for zerop".to_string());
    }

    match &args[0] {
        Object::Integer(n) => Ok(Object::Bool(*n == 0)),
        _ => Err("Argument to zerop must be an integer".to_string()),
    }
}

fn and_function(args: Vec<Object>, _env: &mut Env) -> Result<Object, String> {
    for arg in args {
        let result = eval(arg, &mut Env::new())?;
        if let Object::Bool(false) = result {
            return Ok(Object::Bool(false));
        }
    }

    Ok(Object::Bool(true))
}

fn if_function(args: Vec<Object>, _env: &mut Env) -> Result<Object, String> {
    if args.len() < 2 || args.len() > 3 {
        return Err("Incorrect number of arguments for if".to_string());
    }

    if let Object::Bool(ref bool) = args[0] {
        if *bool {
            return Ok(args[1].clone());
        }

        if args.len() == 3 {
            return Ok(args[2].clone());
        }
    }

    Ok(Object::Bool(false))
}

pub fn dotimes_function(args: Vec<Object>, env: &mut Env) -> Result<Object, String> {
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
        local_env.set(loop_var.clone(), Object::Integer(i));

        let result = eval(body.clone(), &mut local_env);
        if result.is_err() {
            return result;
        }
    }

    Ok(Object::Void())
}

fn cond_function(_args: Vec<Object>, _env: &mut Env) -> Result<Object, String> {
    Ok(Object::Void())
}

pub fn defun_function(args: Vec<Object>, env: &mut Env) -> Result<Object, String> {
    if args.len() != 3 {
        return Err(format!(
            "Incorrect number of arguments for defun want 3 got={}",
            args.len()
        ));
    }

    if let Object::Symbol(ref name) = &args[0] {
        let params = match &args[1] {
            Object::List(list) => list.clone(),
            _ => return Err("Second argument to defun must be a list of parameters".to_string()),
        };

        let body = match &args[2] {
            Object::List(list) => list.clone(),
            _ => {
                return Err(
                    "Third argument to defun must be a list representing the function body"
                        .to_string(),
                )
            }
        };

        env.set(
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

pub fn let_function(args: Vec<Object>, env: &mut Env) -> Result<Object, String> {
    if args.len() != 2 {
        return Err("Incorrect number of arguments for let".to_string());
    }

    if let Object::Symbol(ref name) = args[0] {
        let value = eval(args[1].clone(), env)?;
        env.set(name.clone(), value.clone());
        Ok(value)
    } else {
        Err("First argument to let must be a symbol".to_string())
    }
}
