use crate::lexer::tokenize;
use crate::parser::parse;
use crate::{env::Env, eval::eval_stack};
use std::io::{stdin, Write};

pub fn start() {
    let mut s = String::new();
    let mut env = Env::new();


    print!("{}", handle_input(r#"
    (defun classify_number (n)
    (if (> n 10)
        "Above 10"
        "Under 10"))

    (print (classify_number 1))
"#, &mut env));

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
        Ok(stack) => match eval_stack(stack, env) {
            Ok(result) => result,
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

    #[test]
    fn test_handle_comparisons() {
        // Arrange
        let mut env = Env::new();
        let test_cases = vec![
            // Equality
            ("(= 1 1)", "T"),
            ("(= 1 2)", "NIL"),
            // Inequality
            // to do
            // Greater than
            ("(> 3 2)", "T"),
            ("(> 2 3)", "NIL"),
            ("(> 3 3)", "NIL"),
            // Less than
            ("(< 2 3)", "T"),
            ("(< 3 2)", "NIL"),
            ("(< 3 3)", "NIL"),
        ];

        for (input, expected_output) in test_cases {
            // Act
            let result = handle_input(input, &mut env);

            // Assert
            assert_eq!(result, expected_output, "Failed for input: {}", input);
        }
    }

    #[test]
    fn test_handle_two_char_comparisons() {
        // Arrange
        let mut env = Env::new();
        let test_cases = vec![
            // Inequality
            ("(/= 1 1)", "NIL"),
            ("(/= 1 2)", "T"),
            // Greater than or equal
            ("(>= 3 2)", "T"),
            ("(>= 2 3)", "NIL"),
            ("(>= 3 3)", "T"),
            // Less than or equal comparisons
            ("(<= 2 3)", "T"),
            ("(<= 3 2)", "NIL"),
            ("(<= 3 3)", "T"),
        ];

        for (input, expected_output) in test_cases {
            // Act
            let result = handle_input(input, &mut env);

            // Assert
            assert_eq!(result, expected_output, "Failed for input: {}", input);
        }
    }

    #[test]
    fn test_handle_if() {
        // Arrange
        let mut env = Env::new();
        let test_cases = vec![
            ("(if (> 5 3) 1 0)", "1"),
            ("(if (< 2 1) 10 20)", "20"),
            ("(if (< 7 1) 1)", "NIL"),
            ("(if (>= 1 1) 1)", "1"),
        ];

        for (input, expected_output) in test_cases {
            // Act
            let result = handle_input(input, &mut env);

            // Assert
            assert_eq!(result, expected_output, "Failed for input: {}", input);
        }
    }

    #[test]
    fn test_handle_defun() {
        // Arrange
        let mut env = Env::new();
        let test_cases = vec![
            ("(defun add 
                (a b)
                (+ a b)) 
            (add 1 2)", "3"),
            (r#"
                (defun classify_number (n)
                (if (> n 10)
                    "Above 10"
                    "Under 10"))
    
                (print (classify_number 1))
            "#, "Under 10")
        ];
    
        for (input, expected_output) in test_cases {
            // Act
            let result = handle_input(input, &mut env);
    
            // Debug output
            println!("Input: {}\nResult: {}\nExpected: {}\n", input, result, expected_output);
    
            // Assert
            assert_eq!(result, expected_output, 
                "Failed for input: {}.\nWanted={} \nGot={}", input, expected_output, result);
        }
    }
}
