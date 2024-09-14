#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Integer(i64),
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
            '1'..='9' => {
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
            '+' | '-' | '*' | '/' => {
                tokens.push(Token::Symbol(ch.to_string()));
                chars.next();
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
