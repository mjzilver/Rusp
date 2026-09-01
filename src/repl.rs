use crate::env::Env;
use crate::eval::eval_stack;
use crate::lexer::tokenize;
use crate::parser::parse;
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
    let mut s = String::new();

    println!("Welcome to the Rusp programming language REPL (use exit to exit)");

    loop {
        print!("> ");
        std::io::stdout().flush().expect("Failed to flush stdout");

        s.clear();
        if stdin().read_line(&mut s).is_err() {
            break;
        }

        if s.trim() == "exit" {
            break;
        }

        let response = handle_input(s.trim(), env);
        if !response.is_empty() {
            println!("{}", response);
        }
    }
}

fn handle_file(file_path: &str, env: &mut Rc<RefCell<Env>>) {
    let path = Path::new(file_path);
    let mut file = match File::open(&path) {
        Ok(f) => f,
        Err(e) => {
            eprintln!("Failed to open file {}: {}", file_path, e);
            return;
        }
    };

    let mut content = String::new();
    if let Err(e) = file.read_to_string(&mut content) {
        eprintln!("Failed to read file {}: {}", file_path, e);
        return;
    }

    let response = handle_input(&content, env);
    if !response.is_empty() {
        println!("{}", response);
    }
}

pub fn handle_input(input: &str, env: &mut Rc<RefCell<Env>>) -> String {
    match tokenize(input) {
        Ok(tokens) => match parse(&tokens) {
            Ok(ast) => match eval_stack(ast, env) {
                Ok(result) => result,
                Err(e) => format!("{}", e),
            },
            Err(e) => format!("{}", e),
        },
        Err(e) => format!("{}", e),
    }
}
