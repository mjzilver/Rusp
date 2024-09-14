use std::fmt;

use crate::lexer::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Integer(i64),
    Symbol(String),
    List(Vec<Object>),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Integer(i) => write!(f, "{}", i),
            Object::Symbol(s) => write!(f, "{}", s),
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
        }
    }
}

pub fn parse(tokens: &[Token]) -> Result<Object, String> {
    let mut tokens = tokens.to_vec();
    tokens.reverse();
    parse_list(&mut tokens)
}

fn parse_list(tokens: &mut Vec<Token>) -> Result<Object, String> {
    let mut list = Vec::new();

    while let Some(token) = tokens.pop() {
        match token {
            Token::LParen => {
                let sub_list = parse_list(tokens)?;
                list.push(sub_list);
            }
            Token::RParen => {
                return Ok(Object::List(list));
            }
            Token::Integer(n) => list.push(Object::Integer(n)),
            Token::Symbol(s) => list.push(Object::Symbol(s)),
        }
    }

    if list.len() == 1 {
        Ok(list.pop().unwrap())
    } else {
        Err("Parsing error".to_owned())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add() {
        let mut tokens = Vec::new();
        tokens.push(Token::LParen);
        tokens.push(Token::Symbol("+".to_owned()));
        tokens.push(Token::Integer(2));
        tokens.push(Token::Integer(3));
        tokens.push(Token::RParen);

        let list = parse(&tokens).unwrap();
        assert_eq!(
            list,
            Object::List(vec![
                Object::Symbol("+".to_string()),
                Object::Integer(2),
                Object::Integer(3),
            ])
        );
    }
}
