use std::{cell::RefCell, rc::Rc};

use crate::{
    env::Env,
    eval::{eval, eval_symbol},
    parser::Object,
};

pub type BuiltInFunction = fn(Vec<Object>, env: &mut Rc<RefCell<Env>>) -> Result<Object, String>;

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
        "setq" => Some(setq_function),

        // Lists
        "push" => Some(push_function),
        "reverse" => Some(reverse_function),
        "first" => Some(|args, env| index_list_function(args, env, 0)),
        "second" => Some(|args, env| index_list_function(args, env, 1)),
        "third" => Some(|args, env| index_list_function(args, env, 2)),
        "nth" => Some(nth_function),

        // Control flow
        "if" => Some(if_function),
        "dotimes" => Some(dotimes_function),
        "cond" => Some(cond_function),

        // IO
        "print" => Some(print_function),
        _ => None,
    }
}

fn add_function(args: Vec<Object>, _env: &mut Rc<RefCell<Env>>) -> Result<Object, String> {
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

fn minus_function(args: Vec<Object>, _env: &mut Rc<RefCell<Env>>) -> Result<Object, String> {
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

fn multiply_function(args: Vec<Object>, _env: &mut Rc<RefCell<Env>>) -> Result<Object, String> {
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

fn divide_function(args: Vec<Object>, _env: &mut Rc<RefCell<Env>>) -> Result<Object, String> {
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

fn mod_function(args: Vec<Object>, _env: &mut Rc<RefCell<Env>>) -> Result<Object, String> {
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

fn print_function(args: Vec<Object>, env: &mut Rc<RefCell<Env>>) -> Result<Object, String> {
    if args.is_empty() {
        return Err("No args given to print".to_string());
    }

    for arg in args {
        match arg {
            Object::Integer(n) => println!("{}", n),
            Object::String(s) => println!("{}", s),
            Object::Bool(b) => println!("{}", b),
            Object::DataList(_) => println!("{}", arg.to_string()),
            Object::Symbol(s) => println!("{}", eval_symbol(&s, env)?),
            _ => return Err("Cannot print this type".to_string()),
        }
    }

    Ok(Object::Void())
}

fn not_function(args: Vec<Object>, _env: &mut Rc<RefCell<Env>>) -> Result<Object, String> {
    if args.len() != 1 {
        return Err("Incorrect number of arguments for not".to_string());
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

fn not_equals_all(args: Vec<Object>, _env: &mut Rc<RefCell<Env>>) -> Result<Object, String> {
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

fn zerop_function(args: Vec<Object>, _env: &mut Rc<RefCell<Env>>) -> Result<Object, String> {
    if args.len() != 1 {
        return Err("Incorrect number of arguments for zerop".to_string());
    }

    match &args[0] {
        Object::Integer(n) => Ok(Object::Bool(*n == 0)),
        _ => Err("Argument to zerop must be an integer".to_string()),
    }
}

fn and_function(args: Vec<Object>, env: &mut Rc<RefCell<Env>>) -> Result<Object, String> {
    for arg in args {
        let mut child_env = Rc::new(RefCell::new(Env::new_child(env.clone())));
        let result = eval(arg, &mut child_env)?;
        if let Object::Bool(false) = result {
            return Ok(Object::Bool(false));
        }
    }

    Ok(Object::Bool(true))
}

pub fn push_function(mut args: Vec<Object>, env: &mut Rc<RefCell<Env>>) -> Result<Object, String> {
    if args.len() != 2 {
        return Err("Incorrect number of arguments for push".to_string());
    }

    let obj = match &args[0] {
        Object::Symbol(_) => eval(args.remove(0), env)?,
        Object::Integer(_) | Object::String(_) | Object::Bool(_) => args.remove(0),
        _ => return Err("Cannot add this to list".to_string()),
    };

    let symbol = match &args[0] {
        Object::Symbol(s) => s,
        _ => return Err("Second argument must be a symbol referring to a DataList".to_string()),
    };

    let mut data_list = match env.borrow_mut().get(symbol) {
        Some(Object::DataList(list)) => list,
        _ => return Err("The symbol does not refer to a valid DataList".to_string()),
    };

    data_list.insert(0, obj);

    env.borrow_mut()
        .set(symbol.to_string(), Object::DataList(data_list));

    Ok(env.borrow_mut().get(symbol).unwrap())
}

fn reverse_function(args: Vec<Object>, _env: &mut Rc<RefCell<Env>>) -> Result<Object, String> {
    if args.len() != 1 {
        return Err("Incorrect number of arguments for reverse".to_string());
    }

    let list = match &args[0] {
        Object::DataList(list) => list,
        _ => return Err("Argument must be a DataList".to_string()),
    };
    let mut reversed_list = list.clone();
    reversed_list.reverse();

    Ok(Object::DataList(reversed_list))
}

fn nth_function(args: Vec<Object>, env: &mut Rc<RefCell<Env>>) -> Result<Object, String> {
    if args.len() != 2 {
        return Err("Incorrect number of arguments for nth".to_string());
    }

    let i = match &args[0] {
        Object::Integer(i) => {
            usize::try_from(*i).map_err(|_| "Index must be non-negative".to_string())?
        }
        _ => return Err("Second argument must be an integer".to_string()),
    };

    index_list_function(args[1..].to_vec(), env, i)
}

fn index_list_function(
    args: Vec<Object>,
    _env: &mut Rc<RefCell<Env>>,
    i: usize,
) -> Result<Object, String> {
    if args.len() != 1 {
        return Err("Incorrect number of arguments to index list".to_string());
    }

    let list = match &args[0] {
        Object::DataList(list) => list,
        _ => return Err("Argument must be a DataList".to_string()),
    };

    if i < list.len().try_into().unwrap() {
        Ok(list[i].clone())
    } else {
        Err("Index out of bounds".to_string())
    }
}

// SPECIAL FORM UNDER THIS
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
