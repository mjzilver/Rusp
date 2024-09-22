use std::fmt;

use crate::lexer::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Integer(i64),
    String(String),
    Symbol(String),
    Bool(bool),
    List(Vec<Object>),
    DataList(Vec<Object>),
    Function {
        name: String,
        params: Vec<Object>,
        body: Vec<Object>,
    },
    Void(),
}

// Used for debugging and testing
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
            Object::DataList(data) => {
                write!(f, "(")?;
                let mut first = true;
                for obj in data {
                    if !first {
                        write!(f, " ")?;
                    }
                    first = false;
                    write!(f, "{}", obj)?;
                }
                write!(f, ")")
            }
            Object::Void() => Ok(()),
        }
    }
}

pub fn parse(tokens: &[Token]) -> Result<Vec<Object>, String> {
    let mut tokens = tokens.to_vec();
    tokens.reverse();
    let mut object_stack = Vec::new();

    while !tokens.is_empty() {
        let obj = parse_object(&mut tokens)?;
        object_stack.push(obj);
    }

    Ok(object_stack)
}

fn parse_object(tokens: &mut Vec<Token>) -> Result<Object, String> {
    if let Some(token) = tokens.pop() {
        match token {
            Token::Apostrophe => parse_data_list(tokens),
            Token::LParen => parse_list(tokens),
            Token::RParen => Err("Unexpected closing parenthesis".to_string()),
            Token::Integer(n) => Ok(Object::Integer(n)),
            Token::Symbol(s) => Ok(Object::Symbol(s)),
            Token::String(s) => Ok(Object::String(s)),
        }
    } else {
        Err("Unexpected end of input".to_owned())
    }
}

fn parse_data_list(tokens: &mut Vec<Token>) -> Result<Object, String> {
    let mut data_list = Vec::new();

    while let Some(token) = tokens.pop() {
        match token {
            Token::LParen => (),
            Token::RParen => return Ok(Object::DataList(data_list)),
            Token::Integer(n) => data_list.push(Object::Integer(n)),
            Token::Symbol(s) => data_list.push(Object::Symbol(s)),
            Token::String(s) => data_list.push(Object::String(s)),
            _ => return Err(format!("Cannot put this in a data list: {:?}", token)),
        }
    }

    Ok(Object::DataList(data_list))
}

fn parse_list(tokens: &mut Vec<Token>) -> Result<Object, String> {
    let mut stack = Vec::new();

    while let Some(token) = tokens.pop() {
        match token {
            Token::Apostrophe => {
                let sub_list = parse_data_list(tokens)?;
                stack.push(sub_list);
            }
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

    Err("Parsing error: unmatched parentheses".to_owned())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Token;

    #[test]
    fn test_parse_simple_list() {
        let tokens = vec![
            Token::LParen,
            Token::Integer(42),
            Token::Symbol("x".to_string()),
            Token::String("hello".to_string()),
            Token::RParen,
        ];

        let parsed = parse(&tokens);

        assert!(parsed.is_ok());

        let objects = parsed.unwrap();

        assert_eq!(objects.len(), 1);

        match &objects[0] {
            Object::List(list) => {
                assert_eq!(list.len(), 3);
                assert_eq!(list[0], Object::Integer(42));
                assert_eq!(list[1], Object::Symbol("x".to_string()));
                assert_eq!(list[2], Object::String("hello".to_string()));
            }
            _ => panic!("Expected a list of objects"),
        }
    }

    #[test]
    fn test_parse_nested_list() {
        let tokens = vec![
            Token::LParen,
            Token::Integer(1),
            Token::LParen,
            Token::Integer(2),
            Token::Symbol("y".to_string()),
            Token::RParen,
            Token::RParen,
        ];

        let parsed = parse(&tokens);

        assert!(parsed.is_ok());

        let objects = parsed.unwrap();

        assert_eq!(objects.len(), 1);

        match &objects[0] {
            Object::List(list) => {
                assert_eq!(list.len(), 2);
                assert_eq!(list[0], Object::Integer(1));

                match &list[1] {
                    Object::List(sub_list) => {
                        assert_eq!(sub_list.len(), 2);
                        assert_eq!(sub_list[0], Object::Integer(2));
                        assert_eq!(sub_list[1], Object::Symbol("y".to_string()));
                    }
                    _ => panic!("Expected a nested list"),
                }
            }
            _ => panic!("Expected a list of objects"),
        }
    }

    #[test]
    fn test_parse_data_list() {
        let tokens = vec![
            Token::Apostrophe,
            Token::LParen,
            Token::Symbol("a".to_string()),
            Token::Integer(3),
            Token::RParen,
        ];

        let parsed = parse(&tokens);

        assert!(parsed.is_ok());

        let objects = parsed.unwrap();

        assert_eq!(objects.len(), 1);

        match &objects[0] {
            Object::DataList(data) => {
                assert_eq!(data.len(), 2);
                assert_eq!(data[0], Object::Symbol("a".to_string()));
                assert_eq!(data[1], Object::Integer(3));
            }
            _ => panic!("Expected a data list"),
        }
    }

    #[test]
    fn test_parse_setq_with_datalist() {
        let tokens = vec![
            Token::LParen,
            Token::Symbol("setq".to_string()),
            Token::Symbol("a".to_string()),
            Token::Apostrophe,
            Token::LParen,
            Token::Integer(1),
            Token::Integer(1),
            Token::Integer(1),
            Token::RParen,
            Token::RParen,
        ];

        let parsed = parse(&tokens);

        assert!(parsed.is_ok());

        let objects = parsed.unwrap();

        assert_eq!(objects.len(), 1);

        match &objects[0] {
            Object::List(list) => {
                assert_eq!(list.len(), 3);
                assert_eq!(list[0], Object::Symbol("setq".to_string()));
                assert_eq!(list[1], Object::Symbol("a".to_string()));

                match &list[2] {
                    Object::DataList(data_list) => {
                        assert_eq!(data_list.len(), 3);
                        assert_eq!(data_list[0], Object::Integer(1));
                        assert_eq!(data_list[1], Object::Integer(1));
                        assert_eq!(data_list[2], Object::Integer(1));
                    }
                    _ => panic!("Expected a DataList with (1 1 1)"),
                }
            }
            _ => panic!("Expected a list with setq, a, and a DataList"),
        }
    }
}
