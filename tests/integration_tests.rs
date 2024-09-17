#[cfg(test)]
mod tests {
    use std::{cell::RefCell, rc::Rc};

    use rusp::{handle_input, Env};

    #[test]
    fn test_handle_arithmetic() {
        // Arrange
        let mut env = Rc::new(RefCell::new(Env::new()));
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
        let mut env = Rc::new(RefCell::new(Env::new()));
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
        let mut env = Rc::new(RefCell::new(Env::new()));
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
        let mut env = Rc::new(RefCell::new(Env::new()));
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
        let mut env = Rc::new(RefCell::new(Env::new()));
        let test_cases = vec![
            (
                "(defun add 
                    (a b)
                    (+ a b)) 
                (add 1 2)",
                "3",
            ),
            (
                r#"
                (defun classify_number (n)
                (if (> n 10)
                    "Above 10"
                    "Under 10"))
    
                (classify_number 1)
                "#,
                "\"Under 10\"",
            ),
        ];

        for (input, expected_output) in test_cases {
            // Act
            let result = handle_input(input, &mut env);

            // Assert
            assert_eq!(result, expected_output, "Failed for input: {}", input);
        }
    }

    #[test]
    fn test_fizzbuzz() {
        // Arrange
        let mut env = Rc::new(RefCell::new(Env::new()));
        let test_cases = vec![(
            r#"
                (defun fizzbuzz (n)
                (dotimes (i n)
                    (let ((num (+ i 1)))
                    (cond
                        ((and (zerop (mod num 3)) (zerop (mod num 5))) (print "FizzBuzz"))
                        ((zerop (mod num 3)) (print "Fizz"))
                        ((zerop (mod num 5)) (print "Buzz"))
                        (T (print num))))))

                (fizzbuzz 30)
            "#,
            // if no errors occur = ""
            // TODO find a way to get the proper output
            "",
        )];

        for (input, expected_output) in test_cases {
            // Act
            let result = handle_input(input, &mut env);

            // Assert
            assert_eq!(result, expected_output, "Failed for input: {}", input);
        }
    }

    #[test]
    fn test_recursion() {
        // Arrange
        let mut env = Rc::new(RefCell::new(Env::new()));
        let test_cases = vec![(
            r#"
                (defun sum (n)
                (if (zerop n)
                    0
                    (+ n (sum (- n 1)))))
                (sum 5)
            "#,
            "15",
        )];

        for (input, expected_output) in test_cases {
            // Act
            let result = handle_input(input, &mut env);

            // Assert
            assert_eq!(result, expected_output, "Failed for input: {}", input);
        }
    }
}
