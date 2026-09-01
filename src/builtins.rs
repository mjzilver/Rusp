use std::io::{self, Write};
use std::{cell::RefCell, rc::Rc};

use crate::{args::Args, env::Env, errors::EvalError, value::Value};

pub type PrimitiveFunc = fn(Vec<Value>, &mut Rc<RefCell<Env>>) -> Result<Value, EvalError>;

pub fn get_builtin_function(name: &str) -> Option<PrimitiveFunc> {
    match name {
        // Arithmetic
        "+" => Some(add_function),
        "-" => Some(sub_function),
        "*" => Some(mul_function),
        "/" => Some(div_function),
        "mod" => Some(mod_function),

        // String
        "concat" => Some(concat_function),

        // Comparison & Logic
        "not" => Some(not_function),
        "=" => Some(eq_function),
        "/=" => Some(neq_function),
        ">" => Some(gt_function),
        "<" => Some(lt_function),
        ">=" => Some(gte_function),
        "<=" => Some(lte_function),
        "zero?" => Some(zeroq_function),
        "and" => Some(and_function),
        "or" => Some(or_function),

        // Collections
        "first" => Some(first_function),
        "rest" => Some(rest_function),
        "second" => Some(second_function),
        "third" => Some(third_function),
        "nth" => Some(nth_function),
        "push" | "conj" => Some(conj_function),
        "reverse" => Some(reverse_function),
        "count" => Some(count_function),

        // IO
        "print" => Some(print_function),
        "read-line" => Some(read_line_function),

        _ => None,
    }
}

enum Num {
    Int(i64),
    Float(f64),
}

fn to_num(val: &Value) -> Result<Num, EvalError> {
    match val {
        Value::Integer(i) => Ok(Num::Int(*i)),
        Value::Float(f) => Ok(Num::Float(*f)),
        v => Err(EvalError::TypeMismatch {
            expected: "number".to_string(),
            got: v.type_name().to_string(),
            span: None,
        }),
    }
}

fn add_function(args: Vec<Value>, _env: &mut Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    let mut is_float = false;
    let mut int_sum: i64 = 0;
    let mut float_sum: f64 = 0.0;

    for arg in args {
        match to_num(&arg)? {
            Num::Int(i) => {
                if is_float {
                    float_sum += i as f64;
                } else {
                    int_sum += i;
                }
            }
            Num::Float(f) => {
                if !is_float {
                    is_float = true;
                    float_sum = int_sum as f64 + f;
                } else {
                    float_sum += f;
                }
            }
        }
    }

    if is_float {
        Ok(Value::Float(float_sum))
    } else {
        Ok(Value::Integer(int_sum))
    }
}

fn sub_function(args: Vec<Value>, _env: &mut Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::ArityMismatch {
            expected: "at least 1 argument".to_string(),
            got: 0,
            span: None,
        });
    }

    if args.len() == 1 {
        return match to_num(&args[0])? {
            Num::Int(i) => Ok(Value::Integer(-i)),
            Num::Float(f) => Ok(Value::Float(-f)),
        };
    }

    let first_num = to_num(&args[0])?;
    let mut is_float = matches!(first_num, Num::Float(_));
    let mut int_res = match first_num {
        Num::Int(i) => i,
        Num::Float(f) => f as i64,
    };
    let mut float_res = match first_num {
        Num::Float(f) => f,
        Num::Int(i) => i as f64,
    };

    for arg in &args[1..] {
        match to_num(arg)? {
            Num::Int(i) => {
                if is_float {
                    float_res -= i as f64;
                } else {
                    int_res -= i;
                }
            }
            Num::Float(f) => {
                if !is_float {
                    is_float = true;
                    float_res = int_res as f64 - f;
                } else {
                    float_res -= f;
                }
            }
        }
    }

    if is_float {
        Ok(Value::Float(float_res))
    } else {
        Ok(Value::Integer(int_res))
    }
}

fn mul_function(args: Vec<Value>, _env: &mut Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    let mut is_float = false;
    let mut int_prod: i64 = 1;
    let mut float_prod: f64 = 1.0;

    for arg in args {
        match to_num(&arg)? {
            Num::Int(i) => {
                if is_float {
                    float_prod *= i as f64;
                } else {
                    int_prod *= i;
                }
            }
            Num::Float(f) => {
                if !is_float {
                    is_float = true;
                    float_prod = int_prod as f64 * f;
                } else {
                    float_prod *= f;
                }
            }
        }
    }

    if is_float {
        Ok(Value::Float(float_prod))
    } else {
        Ok(Value::Integer(int_prod))
    }
}

fn div_function(args: Vec<Value>, _env: &mut Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::ArityMismatch {
            expected: "at least 1 argument".to_string(),
            got: 0,
            span: None,
        });
    }

    let first_num = to_num(&args[0])?;
    let mut is_float = matches!(first_num, Num::Float(_));
    let mut int_res = match first_num {
        Num::Int(i) => i,
        Num::Float(f) => f as i64,
    };
    let mut float_res = match first_num {
        Num::Float(f) => f,
        Num::Int(i) => i as f64,
    };

    for arg in &args[1..] {
        match to_num(arg)? {
            Num::Int(i) => {
                if i == 0 {
                    return Err(EvalError::Custom("Division by zero".to_string(), None));
                }
                if is_float {
                    float_res /= i as f64;
                } else {
                    int_res /= i;
                }
            }
            Num::Float(f) => {
                if f == 0.0 {
                    return Err(EvalError::Custom("Division by zero".to_string(), None));
                }
                if !is_float {
                    is_float = true;
                    float_res = int_res as f64 / f;
                } else {
                    float_res /= f;
                }
            }
        }
    }

    if is_float {
        Ok(Value::Float(float_res))
    } else {
        Ok(Value::Integer(int_res))
    }
}

fn mod_function(args: Vec<Value>, _env: &mut Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    let parsed = Args::new(&args);
    parsed.exactly(2, "mod")?;

    let a = parsed.integer(0, "First argument to mod")?;
    let b = parsed.integer(1, "Second argument to mod")?;

    if b == 0 {
        return Err(EvalError::Custom(
            "Division by zero in mod".to_string(),
            None,
        ));
    }

    Ok(Value::Integer(a % b))
}

fn concat_function(args: Vec<Value>, _env: &mut Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    let mut result = String::new();

    for arg in args {
        match arg {
            Value::String(value) => result.push_str(&value),
            val => {
                return Err(EvalError::TypeMismatch {
                    expected: "string".to_string(),
                    got: val.type_name().to_string(),
                    span: None,
                })
            }
        }
    }

    Ok(Value::String(result))
}

fn compare_values<F>(args: Vec<Value>, cmp: F) -> Result<Value, EvalError>
where
    F: Fn(&Value, &Value) -> bool,
{
    let parsed = Args::new(&args);
    parsed.at_least(2, "comparison")?;

    for pair in parsed.args.windows(2) {
        if !cmp(&pair[0], &pair[1]) {
            return Ok(Value::Bool(false));
        }
    }

    Ok(Value::Bool(true))
}

fn eq_function(args: Vec<Value>, _env: &mut Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    compare_values(args, |a, b| a == b)
}

fn neq_function(args: Vec<Value>, _env: &mut Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    compare_values(args, |a, b| a != b)
}

fn num_cmp<F>(a: &Value, b: &Value, cmp_f: F) -> bool
where
    F: Fn(f64, f64) -> bool,
{
    match (to_num(a), to_num(b)) {
        (Ok(Num::Int(i1)), Ok(Num::Int(i2))) => cmp_f(i1 as f64, i2 as f64),
        (Ok(Num::Float(f1)), Ok(Num::Float(f2))) => cmp_f(f1, f2),
        (Ok(Num::Int(i)), Ok(Num::Float(f))) => cmp_f(i as f64, f),
        (Ok(Num::Float(f)), Ok(Num::Int(i))) => cmp_f(f, i as f64),
        _ => false,
    }
}

fn gt_function(args: Vec<Value>, _env: &mut Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    compare_values(args, |a, b| num_cmp(a, b, |x, y| x > y))
}

fn lt_function(args: Vec<Value>, _env: &mut Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    compare_values(args, |a, b| num_cmp(a, b, |x, y| x < y))
}

fn gte_function(args: Vec<Value>, _env: &mut Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    compare_values(args, |a, b| num_cmp(a, b, |x, y| x >= y))
}

fn lte_function(args: Vec<Value>, _env: &mut Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    compare_values(args, |a, b| num_cmp(a, b, |x, y| x <= y))
}

fn not_function(args: Vec<Value>, _env: &mut Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    let parsed = Args::new(&args);
    parsed.exactly(1, "not")?;
    Ok(Value::Bool(!parsed.get(0)?.is_truthy()))
}

fn zeroq_function(args: Vec<Value>, _env: &mut Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    let parsed = Args::new(&args);
    parsed.exactly(1, "zero?")?;
    match to_num(parsed.get(0)?)? {
        Num::Int(i) => Ok(Value::Bool(i == 0)),
        Num::Float(f) => Ok(Value::Bool(f == 0.0)),
    }
}

fn and_function(args: Vec<Value>, _env: &mut Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    let mut last = Value::Bool(true);
    for arg in args {
        if !arg.is_truthy() {
            return Ok(Value::Bool(false));
        }
        last = arg;
    }
    Ok(last)
}

fn or_function(args: Vec<Value>, _env: &mut Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    for arg in args {
        if arg.is_truthy() {
            return Ok(arg);
        }
    }
    Ok(Value::Bool(false))
}

fn first_function(args: Vec<Value>, _env: &mut Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    let parsed = Args::new(&args);
    parsed.exactly(1, "first")?;
    match parsed.get(0)? {
        Value::List(l) => Ok(l.first().cloned().unwrap_or(Value::Nil)),
        Value::Vector(v) => Ok(v.first().cloned().unwrap_or(Value::Nil)),
        val => Err(EvalError::TypeMismatch {
            expected: "list or vector".to_string(),
            got: val.type_name().to_string(),
            span: None,
        }),
    }
}

fn rest_function(args: Vec<Value>, _env: &mut Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    let parsed = Args::new(&args);
    parsed.exactly(1, "rest")?;
    match parsed.get(0)? {
        Value::List(l) => {
            if l.is_empty() {
                Ok(Value::List(vec![]))
            } else {
                Ok(Value::List(l[1..].to_vec()))
            }
        }
        Value::Vector(v) => {
            if v.is_empty() {
                Ok(Value::Vector(vec![]))
            } else {
                Ok(Value::Vector(v[1..].to_vec()))
            }
        }
        val => Err(EvalError::TypeMismatch {
            expected: "list or vector".to_string(),
            got: val.type_name().to_string(),
            span: None,
        }),
    }
}

fn second_function(args: Vec<Value>, _env: &mut Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    let parsed = Args::new(&args);
    parsed.exactly(1, "second")?;
    match parsed.get(0)? {
        Value::List(l) => Ok(l.get(1).cloned().unwrap_or(Value::Nil)),
        Value::Vector(v) => Ok(v.get(1).cloned().unwrap_or(Value::Nil)),
        val => Err(EvalError::TypeMismatch {
            expected: "list or vector".to_string(),
            got: val.type_name().to_string(),
            span: None,
        }),
    }
}

fn third_function(args: Vec<Value>, _env: &mut Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    let parsed = Args::new(&args);
    parsed.exactly(1, "third")?;
    match parsed.get(0)? {
        Value::List(l) => Ok(l.get(2).cloned().unwrap_or(Value::Nil)),
        Value::Vector(v) => Ok(v.get(2).cloned().unwrap_or(Value::Nil)),
        val => Err(EvalError::TypeMismatch {
            expected: "list or vector".to_string(),
            got: val.type_name().to_string(),
            span: None,
        }),
    }
}

fn nth_function(args: Vec<Value>, _env: &mut Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    let parsed = Args::new(&args);
    parsed.exactly(2, "nth")?;
    let idx = parsed.integer(1, "index")?;
    if idx < 0 {
        return Err(EvalError::Custom(
            "Index cannot be negative".to_string(),
            None,
        ));
    }
    let uidx = idx as usize;

    match parsed.get(0)? {
        Value::List(l) => Ok(l.get(uidx).cloned().unwrap_or(Value::Nil)),
        Value::Vector(v) => Ok(v.get(uidx).cloned().unwrap_or(Value::Nil)),
        val => Err(EvalError::TypeMismatch {
            expected: "list or vector".to_string(),
            got: val.type_name().to_string(),
            span: None,
        }),
    }
}

fn conj_function(args: Vec<Value>, _env: &mut Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    let parsed = Args::new(&args);
    parsed.exactly(2, "conj")?;

    match (parsed.get(0)?, parsed.get(1)?) {
        (Value::Vector(v), elem) => {
            let mut new_v = v.clone();
            new_v.push(elem.clone());
            Ok(Value::Vector(new_v))
        }
        (Value::List(l), elem) => {
            let mut new_l = vec![elem.clone()];
            new_l.extend(l.clone());
            Ok(Value::List(new_l))
        }
        // Also support (push elem list) for legacy list push behavior if elem comes second
        (elem, Value::List(l)) => {
            let mut new_l = vec![elem.clone()];
            new_l.extend(l.clone());
            Ok(Value::List(new_l))
        }
        (val, _) => Err(EvalError::TypeMismatch {
            expected: "list or vector".to_string(),
            got: val.type_name().to_string(),
            span: None,
        }),
    }
}

fn reverse_function(args: Vec<Value>, _env: &mut Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    let parsed = Args::new(&args);
    parsed.exactly(1, "reverse")?;
    match parsed.get(0)? {
        Value::List(l) => {
            let mut rev = l.clone();
            rev.reverse();
            Ok(Value::List(rev))
        }
        Value::Vector(v) => {
            let mut rev = v.clone();
            rev.reverse();
            Ok(Value::Vector(rev))
        }
        val => Err(EvalError::TypeMismatch {
            expected: "list or vector".to_string(),
            got: val.type_name().to_string(),
            span: None,
        }),
    }
}

fn count_function(args: Vec<Value>, _env: &mut Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    let parsed = Args::new(&args);
    parsed.exactly(1, "count")?;
    match parsed.get(0)? {
        Value::List(l) => Ok(Value::Integer(l.len() as i64)),
        Value::Vector(v) => Ok(Value::Integer(v.len() as i64)),
        Value::String(s) => Ok(Value::Integer(s.len() as i64)),
        val => Err(EvalError::TypeMismatch {
            expected: "list, vector, or string".to_string(),
            got: val.type_name().to_string(),
            span: None,
        }),
    }
}

fn print_function(args: Vec<Value>, env: &mut Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    let mut outputs = Vec::new();

    for arg in &args {
        let output = match arg {
            Value::String(s) => s.clone(),
            other => format!("{}", other),
        };
        outputs.push(output);
    }

    let joined = outputs.join(" ");
    println!("{}", joined);

    env.borrow_mut().add_output(joined);

    Ok(args.last().cloned().unwrap_or(Value::Void))
}

fn read_line_function(args: Vec<Value>, _env: &mut Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    let parsed = Args::new(&args);
    parsed.exactly(0, "read-line")?;

    let _ = io::stdout().flush();

    let mut buffer = String::new();
    match io::stdin().read_line(&mut buffer) {
        Ok(0) => Ok(Value::Nil),
        Ok(_) => {
            if buffer.ends_with('\n') {
                buffer.pop();
                if buffer.ends_with('\r') {
                    buffer.pop();
                }
            }
            Ok(Value::String(buffer))
        }
        Err(e) => Err(EvalError::Custom(
            format!("Failed to read line: {}", e),
            None,
        )),
    }
}
