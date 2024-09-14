mod lexer;
mod parser;
mod ast;
mod eval;
mod env;
mod repl;

use std::io::stdin;
use parser::parse;

use crate::lexer::tokenize; 

fn main() {
    let mut s = String::new();
    println!("Welcome to the Rusp (Rusted Lisp) programming language REPL (use :q to exit)");

    loop {
        s.clear();
                
        stdin().read_line(&mut s).expect("Incorrect input!");

        if s.trim() == ":q" {
            break;
        }

        let tokens = tokenize(&s.trim());

        match parse(&tokens) {
            Ok(parsed_object) => {
                println!("Parsed Object: {:?}", parsed_object);
            }
            Err(e) => {
                eprintln!("Error: {}", e);
            }
        }

        println!("Tokens: {:?}", tokens);
    }
}