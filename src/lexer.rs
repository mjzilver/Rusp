use crate::errors::{LexError, Span};

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Integer(i64),
    Float(f64),
    String(String),
    Symbol(String),
    Bool(bool),
    Nil,
    LParen,
    RParen,
    LBracket,
    RBracket,
    Apostrophe,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

pub fn tokenize(input: &str) -> Result<Vec<Token>, LexError> {
    let mut tokens = Vec::new();
    let chars: Vec<char> = input.chars().collect();
    let len = chars.len();
    let mut idx = 0;
    let mut line = 1;
    let mut col = 1;

    while idx < len {
        let ch = chars[idx];
        let span = Span::new(line, col);

        match ch {
            '\n' => {
                line += 1;
                col = 1;
                idx += 1;
            }
            c if c.is_whitespace() => {
                col += 1;
                idx += 1;
            }
            ';' => {
                while idx < len && chars[idx] != '\n' {
                    idx += 1;
                }
            }
            '\'' => {
                tokens.push(Token {
                    kind: TokenKind::Apostrophe,
                    span,
                });
                col += 1;
                idx += 1;
            }
            '(' => {
                tokens.push(Token {
                    kind: TokenKind::LParen,
                    span,
                });
                col += 1;
                idx += 1;
            }
            ')' => {
                tokens.push(Token {
                    kind: TokenKind::RParen,
                    span,
                });
                col += 1;
                idx += 1;
            }
            '[' => {
                tokens.push(Token {
                    kind: TokenKind::LBracket,
                    span,
                });
                col += 1;
                idx += 1;
            }
            ']' => {
                tokens.push(Token {
                    kind: TokenKind::RBracket,
                    span,
                });
                col += 1;
                idx += 1;
            }
            '"' => {
                idx += 1;
                col += 1;
                let mut string_val = String::new();
                let mut closed = false;
                while idx < len {
                    let c = chars[idx];
                    if c == '"' {
                        idx += 1;
                        col += 1;
                        closed = true;
                        break;
                    } else if c == '\\' {
                        idx += 1;
                        col += 1;
                        if idx < len {
                            let escaped = match chars[idx] {
                                'n' => '\n',
                                't' => '\t',
                                'r' => '\r',
                                '"' => '"',
                                '\\' => '\\',
                                other => other,
                            };
                            string_val.push(escaped);
                            idx += 1;
                            col += 1;
                        }
                    } else {
                        if c == '\n' {
                            line += 1;
                            col = 1;
                        } else {
                            col += 1;
                        }
                        string_val.push(c);
                        idx += 1;
                    }
                }
                if !closed {
                    return Err(LexError::UnterminatedString(span));
                }
                tokens.push(Token {
                    kind: TokenKind::String(string_val),
                    span,
                });
            }
            '0'..='9' | '-' if is_number_start(&chars, idx) => {
                let start_col = col;
                let start_idx = idx;
                if chars[idx] == '-' {
                    idx += 1;
                    col += 1;
                }
                let mut has_dot = false;
                while idx < len {
                    let c = chars[idx];
                    if c.is_ascii_digit() {
                        idx += 1;
                        col += 1;
                    } else if c == '.'
                        && !has_dot
                        && idx + 1 < len
                        && chars[idx + 1].is_ascii_digit()
                    {
                        has_dot = true;
                        idx += 1;
                        col += 1;
                    } else {
                        break;
                    }
                }
                let num_str: String = chars[start_idx..idx].iter().collect();
                let num_span = Span::new(line, start_col);
                if has_dot {
                    match num_str.parse::<f64>() {
                        Ok(val) => tokens.push(Token {
                            kind: TokenKind::Float(val),
                            span: num_span,
                        }),
                        Err(_) => return Err(LexError::InvalidNumber(num_str, num_span)),
                    }
                } else {
                    match num_str.parse::<i64>() {
                        Ok(val) => tokens.push(Token {
                            kind: TokenKind::Integer(val),
                            span: num_span,
                        }),
                        Err(_) => return Err(LexError::InvalidNumber(num_str, num_span)),
                    }
                }
            }
            _ => {
                let start_col = col;
                let start_idx = idx;
                while idx < len && is_symbol_char(chars[idx]) {
                    idx += 1;
                    col += 1;
                }
                if start_idx == idx {
                    return Err(LexError::UnexpectedChar(chars[idx], span));
                }
                let sym: String = chars[start_idx..idx].iter().collect();
                let sym_span = Span::new(line, start_col);
                let kind = match sym.as_str() {
                    "true" => TokenKind::Bool(true),
                    "false" => TokenKind::Bool(false),
                    "nil" => TokenKind::Nil,
                    _ => TokenKind::Symbol(sym),
                };
                tokens.push(Token {
                    kind,
                    span: sym_span,
                });
            }
        }
    }

    Ok(tokens)
}

fn is_number_start(chars: &[char], idx: usize) -> bool {
    if chars[idx].is_ascii_digit() {
        true
    } else if chars[idx] == '-' && idx + 1 < chars.len() && chars[idx + 1].is_ascii_digit() {
        true
    } else {
        false
    }
}

fn is_symbol_char(c: char) -> bool {
    !c.is_whitespace() && !matches!(c, '(' | ')' | '[' | ']' | '\'' | '"' | ';')
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tokenize_simple_expression() {
        let input = "(+ 2 3.14)";
        let tokens = tokenize(input).unwrap();
        assert_eq!(tokens.len(), 5);
        assert_eq!(tokens[0].kind, TokenKind::LParen);
        assert_eq!(tokens[1].kind, TokenKind::Symbol("+".to_string()));
        assert_eq!(tokens[2].kind, TokenKind::Integer(2));
        assert_eq!(tokens[3].kind, TokenKind::Float(3.14));
        assert_eq!(tokens[4].kind, TokenKind::RParen);
    }

    #[test]
    fn test_tokenize_clojure_features() {
        let input = "[true false nil -10 \"hello\\nworld\"]";
        let tokens = tokenize(input).unwrap();
        assert_eq!(tokens[0].kind, TokenKind::LBracket);
        assert_eq!(tokens[1].kind, TokenKind::Bool(true));
        assert_eq!(tokens[2].kind, TokenKind::Bool(false));
        assert_eq!(tokens[3].kind, TokenKind::Nil);
        assert_eq!(tokens[4].kind, TokenKind::Integer(-10));
        assert_eq!(
            tokens[5].kind,
            TokenKind::String("hello\nworld".to_string())
        );
        assert_eq!(tokens[6].kind, TokenKind::RBracket);
    }
}
