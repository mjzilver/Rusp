use crate::lexer::tokenize;
use crate::parser::parse;
use crate::{env::Env, eval::eval_stack};
use std::cell::RefCell;
use std::io::{stdin, Write};
use std::rc::Rc;

pub fn start() {
    let mut s = String::new();
    let mut env = Rc::new(RefCell::new(Env::new()));

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

pub fn handle_input(input: &str, env: &mut Rc<RefCell<Env>>) -> String {
    let tokens = tokenize(input);

    match parse(&tokens) {
        Ok(stack) => match eval_stack(stack, env) {
            Ok(result) => result,
            Err(e) => format!("Evaluation Error: {}", e),
        },
        Err(e) => format!("Parse Error: {}", e),
    }
}
