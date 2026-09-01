use crate::errors::{EvalError, Span};
use crate::value::Value;

pub struct Args<'a> {
    pub args: &'a [Value],
    pub span: Option<Span>,
}

impl<'a> Args<'a> {
    pub fn new(args: &'a [Value]) -> Self {
        Self { args, span: None }
    }

    pub fn with_span(args: &'a [Value], span: Span) -> Self {
        Self {
            args,
            span: Some(span),
        }
    }

    pub fn exactly(&self, count: usize, _name: &str) -> Result<(), EvalError> {
        if self.args.len() != count {
            return Err(EvalError::ArityMismatch {
                expected: format!("{}", count),
                got: self.args.len(),
                span: self.span,
            });
        }
        Ok(())
    }

    pub fn at_least(&self, count: usize, _name: &str) -> Result<(), EvalError> {
        if self.args.len() < count {
            return Err(EvalError::ArityMismatch {
                expected: format!("at least {}", count),
                got: self.args.len(),
                span: self.span,
            });
        }
        Ok(())
    }

    pub fn get(&self, index: usize) -> Result<&Value, EvalError> {
        self.args.get(index).ok_or_else(|| {
            EvalError::Custom(format!("Missing argument at index {}", index), self.span)
        })
    }

    pub fn symbol(&self, index: usize, _message: &str) -> Result<&str, EvalError> {
        match self.get(index)? {
            Value::Symbol(symbol) => Ok(symbol),
            val => Err(EvalError::TypeMismatch {
                expected: "symbol".to_string(),
                got: val.type_name().to_string(),
                span: self.span,
            }),
        }
    }

    pub fn list(&self, index: usize, _message: &str) -> Result<&Vec<Value>, EvalError> {
        match self.get(index)? {
            Value::List(list) => Ok(list),
            val => Err(EvalError::TypeMismatch {
                expected: "list".to_string(),
                got: val.type_name().to_string(),
                span: self.span,
            }),
        }
    }

    pub fn vector(&self, index: usize, _message: &str) -> Result<&Vec<Value>, EvalError> {
        match self.get(index)? {
            Value::Vector(vec) => Ok(vec),
            val => Err(EvalError::TypeMismatch {
                expected: "vector".to_string(),
                got: val.type_name().to_string(),
                span: self.span,
            }),
        }
    }

    pub fn integer(&self, index: usize, _name: &str) -> Result<i64, EvalError> {
        match self.get(index)? {
            Value::Integer(value) => Ok(*value),
            val => Err(EvalError::TypeMismatch {
                expected: "integer".to_string(),
                got: val.type_name().to_string(),
                span: self.span,
            }),
        }
    }

    pub fn bool(&self, index: usize, _name: &str) -> Result<bool, EvalError> {
        match self.get(index)? {
            Value::Bool(value) => Ok(*value),
            val => Err(EvalError::TypeMismatch {
                expected: "boolean".to_string(),
                got: val.type_name().to_string(),
                span: self.span,
            }),
        }
    }
}
