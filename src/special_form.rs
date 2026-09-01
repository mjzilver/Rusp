use std::{cell::RefCell, rc::Rc};

use crate::{
    env::Env,
    errors::{EvalError, Span},
    eval::eval,
    parser::{Expr, ExprKind},
    value::Value,
};

pub fn eval_special_form(
    name: &str,
    args: &[Expr],
    span: Span,
    env: &mut Rc<RefCell<Env>>,
) -> Result<Option<Value>, EvalError> {
    match name {
        "def" => Ok(Some(def_form(args, span, env)?)),
        "defn" => Ok(Some(defn_form(args, span, env)?)),
        "fn" => Ok(Some(fn_form(args, span, env)?)),
        "let" => Ok(Some(let_form(args, span, env)?)),
        "if" => Ok(Some(if_form(args, span, env)?)),
        "do" => Ok(Some(do_form(args, env)?)),
        "quote" => Ok(Some(quote_form(args, span)?)),
        _ => Ok(None),
    }
}

fn def_form(args: &[Expr], span: Span, env: &mut Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::ArityMismatch {
            expected: "2 (symbol and value)".to_string(),
            got: args.len(),
            span: Some(span),
        });
    }

    let sym_name = match &args[0].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => {
            return Err(EvalError::TypeMismatch {
                expected: "symbol".to_string(),
                got: "non-symbol".to_string(),
                span: Some(args[0].span),
            })
        }
    };

    let val = eval(args[1].clone(), env)?;
    env.borrow_mut().set(sym_name, val.clone());
    Ok(val)
}

fn fn_form(args: &[Expr], span: Span, env: &mut Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::ArityMismatch {
            expected: "at least 1 argument (parameter vector/list)".to_string(),
            got: 0,
            span: Some(span),
        });
    }

    let (name, params_expr, body_exprs) = if let ExprKind::Symbol(n) = &args[0].kind {
        if args.len() < 2 {
            return Err(EvalError::ArityMismatch {
                expected: "at least 2 arguments for named fn".to_string(),
                got: args.len(),
                span: Some(span),
            });
        }
        (Some(n.clone()), &args[1], &args[2..])
    } else {
        (None, &args[0], &args[1..])
    };

    let params = extract_param_names(params_expr)?;

    Ok(Value::Closure {
        name,
        params,
        body: body_exprs.to_vec(),
        env: env.clone(),
    })
}

fn defn_form(args: &[Expr], span: Span, env: &mut Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::ArityMismatch {
            expected: "at least 2 (name and params)".to_string(),
            got: args.len(),
            span: Some(span),
        });
    }

    let fn_name = match &args[0].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => {
            return Err(EvalError::TypeMismatch {
                expected: "symbol".to_string(),
                got: "non-symbol".to_string(),
                span: Some(args[0].span),
            })
        }
    };

    let params = extract_param_names(&args[1])?;
    let body = args[2..].to_vec();

    let closure = Value::Closure {
        name: Some(fn_name.clone()),
        params,
        body,
        env: env.clone(),
    };

    env.borrow_mut().set(fn_name, closure.clone());
    Ok(closure)
}

fn extract_param_names(expr: &Expr) -> Result<Vec<String>, EvalError> {
    let param_exprs = match &expr.kind {
        ExprKind::Vector(v) | ExprKind::List(v) => v,
        _ => {
            return Err(EvalError::TypeMismatch {
                expected: "vector or list of parameters".to_string(),
                got: "non-collection".to_string(),
                span: Some(expr.span),
            })
        }
    };

    let mut names = Vec::new();
    for p in param_exprs {
        match &p.kind {
            ExprKind::Symbol(s) => names.push(s.clone()),
            _ => {
                return Err(EvalError::TypeMismatch {
                    expected: "parameter symbol".to_string(),
                    got: "non-symbol".to_string(),
                    span: Some(p.span),
                })
            }
        }
    }
    Ok(names)
}

fn let_form(args: &[Expr], span: Span, env: &mut Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::ArityMismatch {
            expected: "at least 1 (bindings vector/list)".to_string(),
            got: 0,
            span: Some(span),
        });
    }

    let binding_exprs = match &args[0].kind {
        ExprKind::Vector(v) | ExprKind::List(v) => v,
        _ => {
            return Err(EvalError::TypeMismatch {
                expected: "bindings vector or list".to_string(),
                got: "non-collection".to_string(),
                span: Some(args[0].span),
            })
        }
    };

    if binding_exprs.len() % 2 != 0 {
        return Err(EvalError::Custom(
            "let bindings must contain an even number of forms".to_string(),
            Some(args[0].span),
        ));
    }

    let mut local_env = Rc::new(RefCell::new(Env::new_child(env.clone())));

    for pair in binding_exprs.chunks(2) {
        let sym = match &pair[0].kind {
            ExprKind::Symbol(s) => s.clone(),
            _ => {
                return Err(EvalError::TypeMismatch {
                    expected: "binding symbol".to_string(),
                    got: "non-symbol".to_string(),
                    span: Some(pair[0].span),
                })
            }
        };
        let val = eval(pair[1].clone(), &mut local_env)?;
        local_env.borrow_mut().set(sym, val);
    }

    let mut result = Value::Nil;
    for body_expr in &args[1..] {
        result = eval(body_expr.clone(), &mut local_env)?;
    }
    Ok(result)
}

fn if_form(args: &[Expr], span: Span, env: &mut Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(EvalError::ArityMismatch {
            expected: "2 or 3 (condition, then, optional else)".to_string(),
            got: args.len(),
            span: Some(span),
        });
    }

    let cond_val = eval(args[0].clone(), env)?;
    if cond_val.is_truthy() {
        eval(args[1].clone(), env)
    } else if args.len() == 3 {
        eval(args[2].clone(), env)
    } else {
        Ok(Value::Nil)
    }
}

fn do_form(args: &[Expr], env: &mut Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    let mut last = Value::Nil;
    for expr in args {
        last = eval(expr.clone(), env)?;
    }
    Ok(last)
}

fn quote_form(args: &[Expr], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::ArityMismatch {
            expected: "1".to_string(),
            got: args.len(),
            span: Some(span),
        });
    }
    Ok(ast_to_value(&args[0]))
}

pub fn ast_to_value(expr: &Expr) -> Value {
    match &expr.kind {
        ExprKind::Integer(i) => Value::Integer(*i),
        ExprKind::Float(f) => Value::Float(*f),
        ExprKind::String(s) => Value::String(s.clone()),
        ExprKind::Symbol(s) => Value::Symbol(s.clone()),
        ExprKind::Bool(b) => Value::Bool(*b),
        ExprKind::Nil => Value::Nil,
        ExprKind::List(l) => Value::List(l.iter().map(ast_to_value).collect()),
        ExprKind::Vector(v) => Value::Vector(v.iter().map(ast_to_value).collect()),
        ExprKind::Quote(q) => {
            Value::List(vec![Value::Symbol("quote".to_string()), ast_to_value(q)])
        }
    }
}
