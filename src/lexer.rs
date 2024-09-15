#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Integer(i64),
    String(String),
    Symbol(String),
    LParen,
    RParen,
}

pub fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut chars = input.chars().peekable();

    while let Some(&ch) = chars.peek() {
        match ch {
            '(' => {
                tokens.push(Token::LParen);
                chars.next();
            }
            ')' => {
                tokens.push(Token::RParen);
                chars.next();
            }
            '0'..='9' => {
                let mut number = String::new();
                while let Some(&ch) = chars.peek() {
                    if ch.is_numeric() {
                        number.push(ch);
                        chars.next();
                    } else {
                        break;
                    }
                }
                tokens.push(Token::Integer(number.parse::<i64>().unwrap()));
            }
            _ if ch.is_alphabetic()  => {
                let mut ident = String::new();
                while let Some(&ch) = chars.peek() {
                    if ch.is_alphabetic() || ch == '_' {
                        ident.push(ch);
                        chars.next();
                    } else {
                        break;
                    }
                }
                tokens.push(Token::Symbol(ident))
            }
            '"' => {
                chars.next();
                let mut string = String::new();
                while let Some(&ch) = chars.peek() {
                    if ch == '"' {
                        chars.next();
                        tokens.push(Token::String(string));
                        break;
                    } else {
                        string.push(ch);
                        chars.next();
                    }
                }

                if chars.peek().is_none() {
                    eprintln!("Error: Unclosed string literal");
                }
            }
            _ if is_operator(ch) => {
                let mut op_string = ch.to_string();
                chars.next();

                if let Some(&next_ch) = chars.peek() {
                    if is_two_char_operator(ch, next_ch) {
                        op_string.push(next_ch);
                        chars.next();
                    }
                }

                tokens.push(Token::Symbol(op_string));
            }
            _ if ch.is_whitespace() => {
                chars.next();
            }
            _ => {
                eprintln!("Error: unrecognized token character '{}'", ch);
                chars.next();
            }
        }
    }

    tokens
}

fn is_operator(c: char) -> bool {
    matches!(c, '+' | '-' | '*' | '/' | '<' | '>' | '=')
}

fn is_two_char_operator(first: char, second: char) -> bool {
    matches!((first, second), ('<', '=') | ('>', '=') | ('/', '='))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tokenize_simple_expression() {
        let input = "(+ 2 3)";
        let expected = vec![
            Token::LParen,
            Token::Symbol("+".to_string()),
            Token::Integer(2),
            Token::Integer(3),
            Token::RParen,
        ];
        let tokens = tokenize(input);
        assert_eq!(tokens, expected);
    }
}
