use crate::lexer::tokenize;
use crate::env::Env;
use crate::eval::eval;
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

        let tokens = tokenize(&s.trim());

        match parse(&tokens) {
            Ok(parsed_object) => {
                match eval(parsed_object, &mut env) {
                    Ok(result) => {
                        println!("{}", result);
                    }
                    Err(e) => {
                        eprintln!("Evaluation Error: {}", e);
                    }
                }
            }
            Err(e) => {
                eprintln!("Parse Error: {}", e);
            }
        }
    }
}