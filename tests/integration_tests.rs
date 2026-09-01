#[cfg(test)]
mod tests {
    use std::{cell::RefCell, env, rc::Rc};

    use rusp::{handle_input, Env};

    #[test]
    fn test_handle_arithmetic() {
        env::set_var("DEBUG_MODE", "1");

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
            let result = handle_input(input, &mut env);

            assert_eq!(result, expected_output, "Failed for input: {}", input);
        }
    }

    #[test]
    fn test_handle_comparisons() {
        env::set_var("DEBUG_MODE", "1");

        let mut env = Rc::new(RefCell::new(Env::new()));
        let test_cases = vec![
            // Equality
            ("(= 1 1)", "true"),
            ("(= 1 2)", "false"),
            // Greater than
            ("(> 3 2)", "true"),
            ("(> 2 3)", "false"),
            ("(> 3 3)", "false"),
            // Less than
            ("(< 2 3)", "true"),
            ("(< 3 2)", "false"),
            ("(< 3 3)", "false"),
        ];

        for (input, expected_output) in test_cases {
            let result = handle_input(input, &mut env);

            assert_eq!(result, expected_output, "Failed for input: {}", input);
        }
    }

    #[test]
    fn test_handle_two_char_comparisons() {
        env::set_var("DEBUG_MODE", "1");

        let mut env = Rc::new(RefCell::new(Env::new()));
        let test_cases = vec![
            // Inequality
            ("(/= 1 1)", "false"),
            ("(/= 1 2)", "true"),
            // Greater than or equal
            ("(>= 3 2)", "true"),
            ("(>= 2 3)", "false"),
            ("(>= 3 3)", "true"),
            // Less than or equal comparisons
            ("(<= 2 3)", "true"),
            ("(<= 3 2)", "false"),
            ("(<= 3 3)", "true"),
        ];

        for (input, expected_output) in test_cases {
            let result = handle_input(input, &mut env);

            assert_eq!(result, expected_output, "Failed for input: {}", input);
        }
    }

    #[test]
    fn test_handle_if() {
        env::set_var("DEBUG_MODE", "1");

        let mut env = Rc::new(RefCell::new(Env::new()));
        let test_cases = vec![
            ("(if (> 5 3) 1 0)", "1"),
            ("(if (< 2 1) 10 20)", "20"),
            ("(if (< 7 1) 1)", "nil"),
            ("(if (>= 1 1) 1)", "1"),
        ];

        for (input, expected_output) in test_cases {
            let result = handle_input(input, &mut env);

            assert_eq!(result, expected_output, "Failed for input: {}", input);
        }
    }

    #[test]
    fn test_handle_defn() {
        env::set_var("DEBUG_MODE", "1");

        let mut env = Rc::new(RefCell::new(Env::new()));
        let result = handle_input("(defn add [a b] (+ a b))\n(add 1 2)", &mut env);
        assert!(result.ends_with("3"), "Result was: {}", result);

        let mut env2 = Rc::new(RefCell::new(Env::new()));
        let result2 = handle_input(
            r#"(defn classify_number [n] (if (> n 10) "Above 10" "Under 10")) (classify_number 1)"#,
            &mut env2,
        );
        assert!(result2.ends_with("\"Under 10\""), "Result was: {}", result2);
    }

    #[test]
    fn test_fizzbuzz() {
        env::set_var("DEBUG_MODE", "1");

        let mut env = Rc::new(RefCell::new(Env::new()));

        let input = r#"
            (defn fizzbuzz-step [num]
              (if (and (zero? (mod num 3)) (zero? (mod num 5)))
                (print "FizzBuzz")
                (if (zero? (mod num 3))
                  (print "Fizz")
                  (if (zero? (mod num 5))
                    (print "Buzz")
                    (print num)))))

            (defn fizzbuzz [n current]
              (if (<= current n)
                (do
                  (fizzbuzz-step current)
                  (fizzbuzz n (+ current 1)))
                nil))

            (fizzbuzz 15 1)
        "#;

        handle_input(input, &mut env);
        let output = env.borrow().get_output();

        let expected_lines = vec![
            "1", "2", "Fizz", "4", "Buzz", "Fizz", "7", "8", "Fizz", "Buzz", "11", "Fizz", "13",
            "14", "FizzBuzz",
        ];
        let expected_output = expected_lines.join("\n");

        assert_eq!(output, expected_output, "FizzBuzz output doesn't match");
    }

    #[test]
    fn test_recursion() {
        env::set_var("DEBUG_MODE", "1");

        let mut env = Rc::new(RefCell::new(Env::new()));
        let test_cases = vec![(
            r#"
                (defn sum [n]
                (if (zero? n)
                    0
                    (+ n (sum (- n 1)))))
                (sum 5)
            "#,
            "15",
        )];

        for (input, expected_output) in test_cases {
            let result = handle_input(input, &mut env);

            assert!(
                result.ends_with(expected_output),
                "Failed for input: {}",
                input
            );
        }
    }

    #[test]
    fn test_lexical_closure() {
        env::set_var("DEBUG_MODE", "1");

        let mut env = Rc::new(RefCell::new(Env::new()));
        let input = r#"
            (defn make-adder [x]
              (fn [y] (+ x y)))
            (def add10 (make-adder 10))
            (add10 5)
        "#;

        let result = handle_input(input, &mut env);
        assert!(result.ends_with("15"), "Closure result: {}", result);
    }

    #[test]
    fn test_advanced_closures() {
        env::set_var("DEBUG_MODE", "1");

        let mut env = Rc::new(RefCell::new(Env::new()));

        // Multi-level nested closures with multiple captured variables
        let input1 = r#"
            (defn make-multiplier-adder [factor offset]
              (fn [x]
                (fn [y]
                  (+ (* x factor) (+ y offset)))))
            (def f ((make-multiplier-adder 3 7) 4))
            (f 5)
        "#;
        let result1 = handle_input(input1, &mut env);
        assert!(
            result1.ends_with("24"),
            "Multi-level closure result: {}",
            result1
        ); // (4*3) + (5+7) = 12 + 12 = 24

        // Variable shadowing in closure scope
        let input2 = r#"
            (def x 100)
            (defn shadow-test [x]
              (fn [y] (+ x y)))
            ((shadow-test 5) 10)
        "#;
        let result2 = handle_input(input2, &mut env);
        assert!(
            result2.ends_with("15"),
            "Shadowing closure result: {}",
            result2
        );
    }

    #[test]
    fn test_handle_vector_and_quote() {
        env::set_var("DEBUG_MODE", "1");

        let mut env = Rc::new(RefCell::new(Env::new()));
        let result = handle_input("(def a [1 2 3])", &mut env);
        assert_eq!(result, "[1 2 3]");

        let result2 = handle_input("(def b '(1 2 3))", &mut env);
        assert_eq!(result2, "(1 2 3)");
    }

    #[test]
    fn test_vector_operations() {
        env::set_var("DEBUG_MODE", "1");

        let mut env = Rc::new(RefCell::new(Env::new()));
        let test_cases = vec![
            ("(first [10 20 30])", "10"),
            ("(second [10 20 30])", "20"),
            ("(third [10 20 30])", "30"),
            ("(rest [10 20 30])", "[20 30]"),
            ("(nth [10 20 30] 1)", "20"),
            ("(conj [1 2] 3)", "[1 2 3]"),
            ("(count [1 2 3 4 5])", "5"),
            ("(reverse [1 2 3])", "[3 2 1]"),
            ("(count \"hello\")", "5"),
        ];

        for (input, expected_output) in test_cases {
            let result = handle_input(input, &mut env);
            assert_eq!(result, expected_output, "Failed for input: {}", input);
        }
    }

    #[test]
    fn test_let_bindings() {
        env::set_var("DEBUG_MODE", "1");

        let mut env = Rc::new(RefCell::new(Env::new()));

        let input = "(let [x 10 y (+ x 5) z (* y 2)] (+ x y z))";
        let result = handle_input(input, &mut env);
        assert_eq!(result, "55"); // x=10, y=15, z=30 -> 10+15+30=55
    }

    #[test]
    fn test_float_arithmetic() {
        env::set_var("DEBUG_MODE", "1");

        let mut env = Rc::new(RefCell::new(Env::new()));
        let test_cases = vec![
            ("(+ 1.5 2.5)", "4"),
            ("(* 3 2.5)", "7.5"),
            ("(- 10.0 2.5)", "7.5"),
            ("(/ 5.0 2.0)", "2.5"),
            ("(> 3.14 3.0)", "true"),
        ];

        for (input, expected_output) in test_cases {
            let result = handle_input(input, &mut env);
            assert_eq!(result, expected_output, "Failed for input: {}", input);
        }
    }

    #[test]
    fn test_do_form() {
        env::set_var("DEBUG_MODE", "1");

        let mut env = Rc::new(RefCell::new(Env::new()));
        let input = "(do (def a 1) (def b 2) (+ a b))";
        assert_eq!(handle_input(input, &mut env), "3");
    }

    #[test]
    fn test_logic_and_or() {
        env::set_var("DEBUG_MODE", "1");

        let mut env = Rc::new(RefCell::new(Env::new()));
        let test_cases = vec![
            ("(and true 42)", "42"),
            ("(and false 42)", "false"),
            ("(or false nil \"fallback\")", "\"fallback\""),
            ("(or 10 20)", "10"),
            ("(not false)", "true"),
            ("(not true)", "false"),
            ("(not nil)", "true"),
        ];

        for (input, expected_output) in test_cases {
            let result = handle_input(input, &mut env);
            assert_eq!(result, expected_output, "Failed for input: {}", input);
        }
    }

    #[test]
    fn test_read_line_input() {
        use std::io::Write;
        use std::process::{Command, Stdio};

        let mut child = Command::new(env!("CARGO_BIN_EXE_rusp"))
            .arg("input/user-input.clj")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()
            .expect("Failed to spawn rusp process");

        {
            let stdin = child.stdin.as_mut().expect("Failed to open stdin");
            stdin
                .write_all(b"Hello World from test\n")
                .expect("Failed to write to stdin");
        }

        let output = child.wait_with_output().expect("Failed to wait on child");
        let stdout_str = String::from_utf8_lossy(&output.stdout);

        assert!(
            stdout_str.contains("You entered: Hello World from test"),
            "Expected output to contain prompt result, got:\n{}",
            stdout_str
        );
    }
}
