use std::{cell::RefCell, fmt, rc::Rc};

use crate::env::Env;
use crate::errors::EvalError;
use crate::parser::Expr;

#[derive(Clone)]
pub enum Value {
    Integer(i64),
    Float(f64),
    String(String),
    Symbol(String),
    Bool(bool),
    Nil,
    List(Vec<Value>),
    Vector(Vec<Value>),
    Closure {
        name: Option<String>,
        params: Vec<String>,
        body: Vec<Expr>,
        env: Rc<RefCell<Env>>,
    },
    PrimitiveFunc {
        name: String,
        func: fn(Vec<Value>, &mut Rc<RefCell<Env>>) -> Result<Value, EvalError>,
    },
    Void,
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Symbol(a), Value::Symbol(b)) => a == b,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Nil, Value::Nil) => true,
            (Value::Void, Value::Void) => true,
            (Value::List(a), Value::List(b)) => a == b,
            (Value::Vector(a), Value::Vector(b)) => a == b,
            (Value::PrimitiveFunc { name: a, .. }, Value::PrimitiveFunc { name: b, .. }) => a == b,
            (
                Value::Closure {
                    name: n1,
                    params: p1,
                    body: b1,
                    ..
                },
                Value::Closure {
                    name: n2,
                    params: p2,
                    body: b2,
                    ..
                },
            ) => n1 == n2 && p1 == p2 && b1 == b2,
            _ => false,
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Integer(i) => write!(f, "{}", i),
            Value::Float(fl) => write!(f, "{}", fl),
            Value::Symbol(s) => write!(f, "{}", s),
            Value::String(s) => write!(f, "\"{}\"", s),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Nil => write!(f, "nil"),
            Value::List(lst) => {
                write!(f, "(")?;
                let mut first = true;
                for obj in lst {
                    if !first {
                        write!(f, " ")?;
                    }
                    first = false;
                    write!(f, "{}", obj)?;
                }
                write!(f, ")")
            }
            Value::Vector(vec) => {
                write!(f, "[")?;
                let mut first = true;
                for obj in vec {
                    if !first {
                        write!(f, " ")?;
                    }
                    first = false;
                    write!(f, "{}", obj)?;
                }
                write!(f, "]")
            }
            Value::Closure { name, params, .. } => {
                let fname = name.as_deref().unwrap_or("anonymous");
                write!(f, "#<fn:{}({})>", fname, params.join(" "))
            }
            Value::PrimitiveFunc { name, .. } => {
                write!(f, "#<primitive:{}>", name)
            }
            Value::Void => Ok(()),
        }
    }
}

impl Value {
    pub fn is_truthy(&self) -> bool {
        !matches!(self, Value::Bool(false) | Value::Nil | Value::Void)
    }

    pub fn type_name(&self) -> &'static str {
        match self {
            Value::Integer(_) => "integer",
            Value::Float(_) => "float",
            Value::String(_) => "string",
            Value::Symbol(_) => "symbol",
            Value::Bool(_) => "boolean",
            Value::Nil => "nil",
            Value::List(_) => "list",
            Value::Vector(_) => "vector",
            Value::Closure { .. } => "function",
            Value::PrimitiveFunc { .. } => "primitive-function",
            Value::Void => "void",
        }
    }
}
