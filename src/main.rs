mod builtins;
mod env;
mod eval;
mod lexer;
mod parser;
mod repl;
mod special_form;

fn main() {
    repl::start();
}
