use crate::lexer::tokenize;
use crate::parser::parse;
use crate::{env::Env, eval::eval_stack};
use std::cell::RefCell;
use std::fs::File;
use std::io::{stdin, Read, Write};
use std::path::Path;
use std::rc::Rc;

pub fn start() {
    let mut env = Rc::new(RefCell::new(Env::new()));
    let args: Vec<String> = std::env::args().collect();

    if args.len() > 1 {
        handle_file(&args[1], &mut env)
    } else {
        handle_repl_loop(&mut env)
    }
}

fn handle_repl_loop(env: &mut Rc<RefCell<Env>>) {
    let mut s: String = String::new();

    loop {
        print!("> ");
        std::io::stdout().flush().expect("Failed to flush stdout");

        s.clear();
        stdin().read_line(&mut s).expect("Incorrect input!");

        if s.trim() == ":q" {
            break;
        }

        let response = handle_input(&s.trim(), env);
        println!("{}", response);
    }
}

fn handle_file(file_path: &str, env: &mut Rc<RefCell<Env>>) {
    let path = Path::new(file_path);
    let mut file = File::open(&path).expect("Failed to open file");

    let mut content = String::new();
    file.read_to_string(&mut content)
        .expect("Failed to read file");

    let response = handle_input(&content, env);
    println!("{}", response);
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
