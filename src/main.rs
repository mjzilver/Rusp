mod builtins;
mod env;
mod eval;
mod lexer;
mod parser;
mod repl;

fn main() {
    println!("Welcome to the Rusp (Rusted Lisp) programming language REPL (use :q to exit)");
    repl::start();
}
