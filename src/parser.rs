use std::fmt;

use crate::lexer::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    // Stack holds all the lines in the program
    Stack(Vec<Object>),

    Integer(i64),
    String(String),
    Symbol(String),
    Bool(bool),
    List(Vec<Object>),
    Function {
        name: String,
        params: Vec<Object>,
        body: Vec<Object>,
    },
    Void(),
}

// Used by the print function and debugging
impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Integer(i) => write!(f, "{}", i),
            Object::Symbol(s) => write!(f, "{}", s),
            Object::String(s) => write!(f, "\"{}\"", s),
            Object::Bool(b) => write!(f, "{}", if *b { "T" } else { "NIL" }),
            Object::List(lst) => {
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
            Object::Function { name, params, body } => {
                write!(f, "Function: {}\n", name)?;
                write!(f, "Parameters: [")?;
                let mut first = true;
                for param in params {
                    if !first {
                        write!(f, ", ")?;
                    }
                    first = false;
                    write!(f, "{}", param)?;
                }
                write!(f, "]\n")?;
                write!(f, "Body: (")?;
                let mut first = true;
                for expr in body {
                    if !first {
                        write!(f, " ")?;
                    }
                    first = false;
                    write!(f, "{}", expr)?;
                }
                write!(f, ")")
            }
            Object::Void() => Ok(()),
            Object::Stack(_) => panic!("Cannot output stack"),
        }
    }
}

pub fn parse(tokens: &[Token]) -> Result<Object, String> {
    let mut tokens = tokens.to_vec();
    tokens.reverse();
    parse_list(&mut tokens)
}

fn parse_list(tokens: &mut Vec<Token>) -> Result<Object, String> {
    let mut stack = Vec::new();

    while let Some(token) = tokens.pop() {
        match token {
            Token::LParen => {
                let sub_list = parse_list(tokens)?;
                stack.push(sub_list);
            }
            Token::RParen => {
                return Ok(Object::List(stack));
            }
            Token::Integer(n) => stack.push(Object::Integer(n)),
            Token::Symbol(s) => stack.push(Object::Symbol(s)),
            Token::String(s) => stack.push(Object::String(s)),
        }
    }

    if stack.len() != 0 {
        Ok(Object::Stack(stack))
    } else {
        Err("Parsing error".to_owned())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parsing_single_list() {
        let mut tokens = Vec::new();
        tokens.push(Token::LParen);
        tokens.push(Token::Symbol("+".to_owned()));
        tokens.push(Token::Integer(2));
        tokens.push(Token::Integer(3));
        tokens.push(Token::RParen);

        let list = parse(&tokens).unwrap();
        assert_eq!(
            list,
            Object::Stack(vec![Object::List(vec![
                Object::Symbol("+".to_string()),
                Object::Integer(2),
                Object::Integer(3),
            ])])
        );
    }
}
