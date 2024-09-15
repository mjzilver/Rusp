use crate::builtins::{self, get_builtin_function};
use crate::{env::Env, parser::Object};

pub fn eval(object: Object, env: &mut Env) -> Result<Object, String> {
    match object {
        Object::Integer(_) => Ok(object),
        Object::String(_) => Ok(object),
        Object::Bool(_) => Ok(object),
        Object::Symbol(ref s) => eval_symbol(s, env),
        Object::List(ref list) => eval_list(list, env),
        Object::Void() => Ok(Object::Void()),
    }
}

fn eval_symbol(s: &String, env: &mut Env) -> Result<Object, String> {
    if let Some(_) = get_builtin_function(s) {
        return Ok(Object::Symbol(s.clone()));
    }

    env.get(s)
        .cloned()
        .ok_or_else(|| format!("Undefined symbol: {}", s))
}

fn eval_list(list: &Vec<Object>, env: &mut Env) -> Result<Object, String> {
    if list.is_empty() {
        return Err("Empty list".to_string());
    }

    let func = eval(list[0].clone(), env)?;
    let args = &list[1..];

    if let Object::Symbol(ref s) = func {
        if s == "let" {
            return builtins::let_function(args.to_vec(), env)
        }
    }

    let args = args
        .iter()
        .map(|arg| eval(arg.clone(), env))
        .collect::<Result<Vec<_>, _>>()?;

    apply_function(func, args, env)
}

fn apply_function(func: Object, args: Vec<Object>, env: &mut Env) -> Result<Object, String> {
    match func {
        Object::Symbol(ref s) => {
            if let Some(built_in) = get_builtin_function(s.as_str()) {
                built_in(args, env)
            } else {
                Err(format!("Unknown function: {}", s))
            }
        }
        _ => Err("Function application on non-function".to_string()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::env::Env;
    use crate::parser::Object;

    #[test]
    fn test_eval_addition() {
        // Arrange
        let mut env = Env::new();
        let input = Object::List(vec![
            Object::Symbol("+".to_string()),
            Object::Integer(1),
            Object::Integer(2),
        ]);

        // Act
        let result = eval(input, &mut env);

        // Assert
        let expected = Ok(Object::Integer(3));
        assert_eq!(result, expected);
    }
}
