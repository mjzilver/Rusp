use std::cell::RefCell;
use std::rc::Rc;

use crate::builtins::get_builtin_function;
use crate::env::Env;
use crate::errors::{EvalError, RuspResult, Span};
use crate::parser::{Expr, ExprKind};
use crate::special_form::{ast_to_value, eval_special_form};
use crate::value::Value;

pub fn eval(expr: Expr, env: &mut Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    let span = expr.span;
    match expr.kind {
        ExprKind::Integer(i) => Ok(Value::Integer(i)),
        ExprKind::Float(f) => Ok(Value::Float(f)),
        ExprKind::String(s) => Ok(Value::String(s)),
        ExprKind::Bool(b) => Ok(Value::Bool(b)),
        ExprKind::Nil => Ok(Value::Nil),
        ExprKind::Quote(q) => Ok(ast_to_value(&q)),
        ExprKind::Symbol(s) => eval_symbol(&s, span, env),
        ExprKind::Vector(elements) => {
            let mut evaluated = Vec::new();
            for item in elements {
                evaluated.push(eval(item, env)?);
            }
            Ok(Value::Vector(evaluated))
        }
        ExprKind::List(elements) => eval_list(&elements, span, env),
    }
}

pub fn eval_stack(ast: Vec<Expr>, env: &mut Rc<RefCell<Env>>) -> RuspResult<String> {
    let mut outputs = Vec::new();

    for expr in ast {
        let val = eval(expr, env)?;
        if std::env::var("DEBUG_MODE").is_ok() && val != Value::Void {
            outputs.push(format!("{}", val));
        }
    }

    Ok(outputs.join("\n"))
}

pub fn eval_symbol(s: &str, span: Span, env: &mut Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    if let Some(val) = env.borrow().get(s) {
        return Ok(val);
    }
    if let Some(primitive) = get_builtin_function(s) {
        return Ok(Value::PrimitiveFunc {
            name: s.to_string(),
            func: primitive,
        });
    }

    Err(EvalError::UnboundSymbol(s.to_string(), Some(span)))
}

fn eval_list(
    elements: &[Expr],
    span: Span,
    env: &mut Rc<RefCell<Env>>,
) -> Result<Value, EvalError> {
    if elements.is_empty() {
        return Ok(Value::List(vec![]));
    }

    if let ExprKind::Symbol(ref sym) = elements[0].kind {
        if let Some(res) = eval_special_form(sym, &elements[1..], span, env)? {
            return Ok(res);
        }
    }

    let func = eval(elements[0].clone(), env)?;
    apply_function(func, &elements[1..], span, env)
}

fn apply_function(
    func: Value,
    arg_exprs: &[Expr],
    span: Span,
    caller_env: &mut Rc<RefCell<Env>>,
) -> Result<Value, EvalError> {
    match func {
        Value::PrimitiveFunc {
            func: primitive, ..
        } => {
            let mut eval_args = Vec::new();
            for arg_expr in arg_exprs {
                eval_args.push(eval(arg_expr.clone(), caller_env)?);
            }
            primitive(eval_args, caller_env)
        }
        Value::Closure {
            name: _,
            params,
            body,
            env: closure_env,
        } => {
            let mut eval_args = Vec::new();
            for arg_expr in arg_exprs {
                eval_args.push(eval(arg_expr.clone(), caller_env)?);
            }

            if params.len() != eval_args.len() {
                return Err(EvalError::ArityMismatch {
                    expected: format!("{}", params.len()),
                    got: eval_args.len(),
                    span: Some(span),
                });
            }

            let mut local_env = Rc::new(RefCell::new(Env::new_child(closure_env.clone())));
            for (param, arg) in params.into_iter().zip(eval_args) {
                local_env.borrow_mut().set(param, arg);
            }

            let mut last_result = Value::Nil;
            for stmt in body {
                last_result = eval(stmt, &mut local_env)?;
            }
            Ok(last_result)
        }
        other => Err(EvalError::TypeMismatch {
            expected: "function".to_string(),
            got: other.type_name().to_string(),
            span: Some(span),
        }),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::env::Env;

    #[test]
    fn test_eval_addition() {
        let mut env = Rc::new(RefCell::new(Env::new()));
        let expr = Expr {
            kind: ExprKind::List(vec![
                Expr {
                    kind: ExprKind::Symbol("+".to_string()),
                    span: Span::new(1, 1),
                },
                Expr {
                    kind: ExprKind::Integer(1),
                    span: Span::new(1, 3),
                },
                Expr {
                    kind: ExprKind::Integer(2),
                    span: Span::new(1, 5),
                },
            ]),
            span: Span::new(1, 1),
        };

        let result = eval(expr, &mut env);
        assert_eq!(result, Ok(Value::Integer(3)));
    }
}
