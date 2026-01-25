mod builtins;
mod env;
mod eval;
mod lexer;
mod parser;
mod repl;
mod special_form;

fn main() {
    println!("Welcome to the Rusp programming language REPL (use :q to exit)");
    repl::start();
}
