use std::fmt;

use crate::errors::{ParseError, Span};
use crate::lexer::{Token, TokenKind};

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Integer(i64),
    Float(f64),
    String(String),
    Symbol(String),
    Bool(bool),
    Nil,
    List(Vec<Expr>),
    Vector(Vec<Expr>),
    Quote(Box<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            ExprKind::Integer(i) => write!(f, "{}", i),
            ExprKind::Float(fl) => write!(f, "{}", fl),
            ExprKind::String(s) => write!(f, "\"{}\"", s),
            ExprKind::Symbol(s) => write!(f, "{}", s),
            ExprKind::Bool(b) => write!(f, "{}", b),
            ExprKind::Nil => write!(f, "nil"),
            ExprKind::List(lst) => {
                write!(f, "(")?;
                let mut first = true;
                for item in lst {
                    if !first {
                        write!(f, " ")?;
                    }
                    first = false;
                    write!(f, "{}", item)?;
                }
                write!(f, ")")
            }
            ExprKind::Vector(vec) => {
                write!(f, "[")?;
                let mut first = true;
                for item in vec {
                    if !first {
                        write!(f, " ")?;
                    }
                    first = false;
                    write!(f, "{}", item)?;
                }
                write!(f, "]")
            }
            ExprKind::Quote(expr) => write!(f, "'{}", expr),
        }
    }
}

pub fn parse(tokens: &[Token]) -> Result<Vec<Expr>, ParseError> {
    let mut tokens = tokens.to_vec();
    tokens.reverse();
    let mut ast = Vec::new();

    while !tokens.is_empty() {
        let expr = parse_expr(&mut tokens)?;
        ast.push(expr);
    }

    Ok(ast)
}

fn parse_expr(tokens: &mut Vec<Token>) -> Result<Expr, ParseError> {
    let token = tokens.pop().ok_or(ParseError::UnexpectedEof)?;
    let span = token.span;

    match token.kind {
        TokenKind::Apostrophe => {
            let quoted = parse_expr(tokens)?;
            let q_span = quoted.span;
            Ok(Expr {
                kind: ExprKind::Quote(Box::new(quoted)),
                span: q_span,
            })
        }
        TokenKind::LParen => parse_list(tokens, span),
        TokenKind::RParen => Err(ParseError::UnmatchedParen(span)),
        TokenKind::LBracket => parse_vector(tokens, span),
        TokenKind::RBracket => Err(ParseError::UnmatchedBracket(span)),
        TokenKind::Integer(n) => Ok(Expr {
            kind: ExprKind::Integer(n),
            span,
        }),
        TokenKind::Float(fl) => Ok(Expr {
            kind: ExprKind::Float(fl),
            span,
        }),
        TokenKind::String(s) => Ok(Expr {
            kind: ExprKind::String(s),
            span,
        }),
        TokenKind::Symbol(s) => Ok(Expr {
            kind: ExprKind::Symbol(s),
            span,
        }),
        TokenKind::Bool(b) => Ok(Expr {
            kind: ExprKind::Bool(b),
            span,
        }),
        TokenKind::Nil => Ok(Expr {
            kind: ExprKind::Nil,
            span,
        }),
    }
}

fn parse_list(tokens: &mut Vec<Token>, start_span: Span) -> Result<Expr, ParseError> {
    let mut elements = Vec::new();

    while let Some(peeked) = tokens.last() {
        if peeked.kind == TokenKind::RParen {
            tokens.pop();
            return Ok(Expr {
                kind: ExprKind::List(elements),
                span: start_span,
            });
        }
        elements.push(parse_expr(tokens)?);
    }

    Err(ParseError::UnmatchedParen(start_span))
}

fn parse_vector(tokens: &mut Vec<Token>, start_span: Span) -> Result<Expr, ParseError> {
    let mut elements = Vec::new();

    while let Some(peeked) = tokens.last() {
        if peeked.kind == TokenKind::RBracket {
            tokens.pop();
            return Ok(Expr {
                kind: ExprKind::Vector(elements),
                span: start_span,
            });
        }
        elements.push(parse_expr(tokens)?);
    }

    Err(ParseError::UnmatchedBracket(start_span))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::{Token, TokenKind};

    fn make_tok(kind: TokenKind) -> Token {
        Token {
            kind,
            span: Span::new(1, 1),
        }
    }

    #[test]
    fn test_parse_simple_list() {
        let tokens = vec![
            make_tok(TokenKind::LParen),
            make_tok(TokenKind::Integer(42)),
            make_tok(TokenKind::Symbol("x".to_string())),
            make_tok(TokenKind::String("hello".to_string())),
            make_tok(TokenKind::RParen),
        ];

        let parsed = parse(&tokens);
        assert!(parsed.is_ok());

        let exprs = parsed.unwrap();
        assert_eq!(exprs.len(), 1);

        match &exprs[0].kind {
            ExprKind::List(list) => {
                assert_eq!(list.len(), 3);
                assert_eq!(list[0].kind, ExprKind::Integer(42));
                assert_eq!(list[1].kind, ExprKind::Symbol("x".to_string()));
                assert_eq!(list[2].kind, ExprKind::String("hello".to_string()));
            }
            _ => panic!("Expected a list of exprs"),
        }
    }

    #[test]
    fn test_parse_nested_list() {
        let tokens = vec![
            make_tok(TokenKind::LParen),
            make_tok(TokenKind::Integer(1)),
            make_tok(TokenKind::LParen),
            make_tok(TokenKind::Integer(2)),
            make_tok(TokenKind::Symbol("y".to_string())),
            make_tok(TokenKind::RParen),
            make_tok(TokenKind::RParen),
        ];

        let parsed = parse(&tokens);
        assert!(parsed.is_ok());

        let exprs = parsed.unwrap();
        assert_eq!(exprs.len(), 1);

        match &exprs[0].kind {
            ExprKind::List(list) => {
                assert_eq!(list.len(), 2);
                assert_eq!(list[0].kind, ExprKind::Integer(1));

                match &list[1].kind {
                    ExprKind::List(sub_list) => {
                        assert_eq!(sub_list.len(), 2);
                        assert_eq!(sub_list[0].kind, ExprKind::Integer(2));
                        assert_eq!(sub_list[1].kind, ExprKind::Symbol("y".to_string()));
                    }
                    _ => panic!("Expected a nested list"),
                }
            }
            _ => panic!("Expected a list of exprs"),
        }
    }

    #[test]
    fn test_parse_quote() {
        let tokens = vec![
            make_tok(TokenKind::Apostrophe),
            make_tok(TokenKind::LParen),
            make_tok(TokenKind::Symbol("a".to_string())),
            make_tok(TokenKind::Integer(3)),
            make_tok(TokenKind::RParen),
        ];

        let parsed = parse(&tokens);
        assert!(parsed.is_ok());

        let exprs = parsed.unwrap();
        assert_eq!(exprs.len(), 1);

        match &exprs[0].kind {
            ExprKind::Quote(q) => match &q.kind {
                ExprKind::List(data) => {
                    assert_eq!(data.len(), 2);
                    assert_eq!(data[0].kind, ExprKind::Symbol("a".to_string()));
                    assert_eq!(data[1].kind, ExprKind::Integer(3));
                }
                _ => panic!("Expected list inside quote"),
            },
            _ => panic!("Expected quote expression"),
        }
    }
}
