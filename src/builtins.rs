use std::{cell::RefCell, rc::Rc};

use crate::{
    args::Args,
    env::Env,
    eval::{eval, eval_symbol},
    parser::Object,
    special_form::*,
};

pub type BuiltInFunction = fn(Vec<Object>, env: &mut Rc<RefCell<Env>>) -> Result<Object, String>;

pub fn get_builtin_function(name: &str) -> Option<BuiltInFunction> {
    match name {
        // Arithmetic
        "+" => Some(|args, env| arithmetic_function(args, env, |a, b| a + b)),
        "-" => Some(|args, env| arithmetic_function(args, env, |a, b| a - b)),
        "*" => Some(|args, env| arithmetic_function(args, env, |a, b| a * b)),
        "/" => Some(|args, env| arithmetic_function(args, env, |a, b| a / b)),
        "mod" => Some(mod_function),

        // String
        "concat" => Some(concat_function),

        // Comparison
        "not" => Some(not_function),
        "=" => Some(|args, _| compare_objects(args, |a, b| a == b)),
        "/=" => Some(not_equals_all),
        ">" => Some(|args, _| compare_objects(args, |a, b| a > b)),
        "<" => Some(|args, _| compare_objects(args, |a, b| a < b)),
        ">=" => Some(|args, _| compare_objects(args, |a, b| a >= b)),
        "<=" => Some(|args, _| compare_objects(args, |a, b| a <= b)),
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

fn integer_from_object(object: &Object, name: &str) -> Result<i64, String> {
    match object {
        Object::Integer(value) => Ok(*value),
        _ => Err(format!("{name} must be an integer")),
    }
}

fn arithmetic_function(
    args: Vec<Object>,
    _env: &mut Rc<RefCell<Env>>,
    operator: fn(i64, i64) -> i64,
) -> Result<Object, String> {
    let (first, rest) = args
        .split_first()
        .ok_or_else(|| "First argument must be an integer".to_string())?;

    let mut result = integer_from_object(first, "First argument")?;

    for arg in rest {
        result = operator(result, integer_from_object(arg, "Argument")?);
    }

    Ok(Object::Integer(result))
}

fn mod_function(args: Vec<Object>, _env: &mut Rc<RefCell<Env>>) -> Result<Object, String> {
    let args = Args::new(&args);
    args.exactly(2, "mod")?;

    let a = args.integer(0, "First argument to mod")?;
    let b = args.integer(1, "Second argument to mod")?;

    if b == 0 {
        return Err("Division by zero in mod".to_string());
    }

    Ok(Object::Integer(a % b))
}

fn concat_function(args: Vec<Object>, _env: &mut Rc<RefCell<Env>>) -> Result<Object, String> {
    let mut result = String::new();

    for arg in args {
        match arg {
            Object::String(value) => result.push_str(&value),
            _ => return Err("Cannot concatenate non-string values".to_string()),
        }
    }

    Ok(Object::String(result))
}

type CompareFn = fn(&Object, &Object) -> bool;

fn compare_objects(args: Vec<Object>, comparison: CompareFn) -> Result<Object, String> {
    let args = Args::new(&args);
    args.at_least(2, "Require at least 2 items to compare")?;

    for pair in args.args.windows(2) {
        let left = &pair[0];
        let right = &pair[1];

        if !same_comparable_type(left, right) {
            return Err("Unsupported comparison between different types".to_string());
        }

        if !comparison(left, right) {
            return Ok(Object::Bool(false));
        }
    }

    Ok(Object::Bool(true))
}

fn same_comparable_type(left: &Object, right: &Object) -> bool {
    matches!(
        (left, right),
        (Object::Bool(_), Object::Bool(_))
            | (Object::Integer(_), Object::Integer(_))
            | (Object::String(_), Object::String(_))
    )
}

fn equals(left: &Object, right: &Object) -> bool {
    match (left, right) {
        (Object::Bool(a), Object::Bool(b)) => a == b,
        (Object::Integer(a), Object::Integer(b)) => a == b,
        (Object::String(a), Object::String(b)) => a == b,
        _ => false,
    }
}

fn not_equals_all(args: Vec<Object>, _env: &mut Rc<RefCell<Env>>) -> Result<Object, String> {
    let args = Args::new(&args);
    args.at_least(2, "Require at least 2 items to compare")?;

    for (i, left) in args.args.iter().enumerate() {
        for right in &args.args[i + 1..] {
            if equals(left, right) {
                return Ok(Object::Bool(false));
            }
        }
    }

    Ok(Object::Bool(true))
}

fn not_function(args: Vec<Object>, _env: &mut Rc<RefCell<Env>>) -> Result<Object, String> {
    let args = Args::new(&args);
    args.exactly(1, "not")?;

    Ok(Object::Bool(!args.bool(0, "Argument to not")?))
}

fn zerop_function(args: Vec<Object>, _env: &mut Rc<RefCell<Env>>) -> Result<Object, String> {
    let args = Args::new(&args);
    args.exactly(1, "zerop")?;

    Ok(Object::Bool(args.integer(0, "Argument to zerop")? == 0))
}

fn and_function(args: Vec<Object>, env: &mut Rc<RefCell<Env>>) -> Result<Object, String> {
    for arg in args {
        let mut child_env = Rc::new(RefCell::new(Env::new_child(env.clone())));
        let result = eval(arg, &mut child_env)?;

        if matches!(result, Object::Bool(false)) {
            return Ok(Object::Bool(false));
        }
    }

    Ok(Object::Bool(true))
}

pub fn push_function(mut args: Vec<Object>, env: &mut Rc<RefCell<Env>>) -> Result<Object, String> {
    let validated = Args::new(&args);
    validated.exactly(2, "push")?;

    let value = match args.remove(0) {
        Object::Symbol(_) => eval(args[0].clone(), env)?,
        value
        @ (Object::Integer(_) | Object::String(_) | Object::Bool(_) | Object::DataList(_)) => value,
        _ => return Err("Cannot add this to list".to_string()),
    };

    let symbol = match &args[0] {
        Object::Symbol(symbol) => symbol,
        _ => return Err("Second argument must be a symbol referring to a DataList".to_string()),
    };

    let mut list = match env.borrow().get(symbol) {
        Some(Object::DataList(list)) => list,
        _ => return Err("The symbol does not refer to a valid DataList".to_string()),
    };

    list.insert(0, value);

    let result = Object::DataList(list);

    env.borrow_mut().set(symbol.to_string(), result.clone());

    Ok(result)
}

fn reverse_function(args: Vec<Object>, _env: &mut Rc<RefCell<Env>>) -> Result<Object, String> {
    let args = Args::new(&args);
    args.exactly(1, "reverse")?;

    let mut list = args.list(0, "reverse")?.clone();
    list.reverse();

    Ok(Object::DataList(list))
}

fn nth_function(args: Vec<Object>, env: &mut Rc<RefCell<Env>>) -> Result<Object, String> {
    let args = Args::new(&args);
    args.exactly(2, "nth")?;

    let index = args.integer(0, "Index")?;

    let index = usize::try_from(index).map_err(|_| "Index must be non-negative".to_string())?;

    index_list_function(vec![args.get(1)?.clone()], env, index)
}

fn index_list_function(
    args: Vec<Object>,
    _env: &mut Rc<RefCell<Env>>,
    index: usize,
) -> Result<Object, String> {
    let args = Args::new(&args);
    args.exactly(1, "index list")?;

    args.list(0, "index list")?
        .get(index)
        .cloned()
        .ok_or_else(|| "Index out of bounds".to_string())
}

fn print_function(args: Vec<Object>, env: &mut Rc<RefCell<Env>>) -> Result<Object, String> {
    if args.is_empty() {
        return Err("No args given to print".to_string());
    }

    for arg in &args {
        let output = match arg {
            Object::Integer(value) => value.to_string(),
            Object::String(value) => value.clone(),
            Object::Bool(value) => value.to_string(),
            Object::DataList(_) => arg.to_string(),
            Object::Symbol(symbol) => eval_symbol(symbol, env)?.to_string(),
            _ => return Err("Cannot print this type".to_string()),
        };

        println!("{output}");

        #[cfg(any(test, feature = "test-helpers"))]
        env.borrow_mut().add_output(output);
    }

    Ok(args.last().unwrap().clone())
}
