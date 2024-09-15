use crate::env::Env;
use crate::eval::eval;
use crate::lexer::tokenize;
use crate::parser::parse;
use std::io::{stdin, Write};

pub fn start() {
    let mut s = String::new();
    let mut env = Env::new();

    loop {
        print!("> ");
        std::io::stdout().flush().expect("Failed to flush stdout");

        s.clear();
        stdin().read_line(&mut s).expect("Incorrect input!");

        if s.trim() == ":q" {
            break;
        }

        let response = handle_input(&s.trim(), &mut env);
        println!("{}", response);
    }
}

pub fn handle_input(input: &str, env: &mut Env) -> String {
    let tokens = tokenize(input);

    match parse(&tokens) {
        Ok(parsed_object) => match eval(parsed_object, env) {
            Ok(result) => result.to_string(),
            Err(e) => format!("Evaluation Error: {}", e),
        },
        Err(e) => format!("Parse Error: {}", e),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_handle_arithmetic() {
        // Arrange
        let mut env = Env::new();
        let test_cases = vec![
            // Addition
            ("(+ 1 2)", "3"),
            ("(+ 1 2 3)", "6"),
            ("(+ 1 (+ 2 3))", "6"),
            // Subtraction
            ("(- 3 1)", "2"),
            ("(- 10 3 2)", "5"),
            ("(- (- 10 3) 2)", "5"),
            // Multiplication
            ("(* 2 3)", "6"),
            ("(* 2 3 4)", "24"),
            ("(* 2 (+ 3 4))", "14"),
            // Division
            ("(/ 6 3)", "2"),
            ("(/ 24 3 2)", "4"),
            ("(/ (/ 24 3) 2)", "4"),
            // Nested and Mixed Expressions
            ("(+ (* 2 3) (- 10 5))", "11"),
            ("(* (- 6 2) (/ 12 3))", "16"),
            ("(/ (+ 10 2) (- 5 1))", "3"),
        ];

        for (input, expected_output) in test_cases {
            // Act
            let result = handle_input(input, &mut env);

            // Assert
            assert_eq!(result, expected_output, "Failed for input: {}", input);
        }
    }
}
