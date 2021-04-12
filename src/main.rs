mod ast;
mod lexer {
    pub mod lex;
}
mod parser {
    pub mod parse;
    pub mod parselets;
}
mod interpreter {
    pub mod interpret;
}

use interpreter::interpret;
use lexer::lex;
use logos::Logos;
use parser::parse;
use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("============================================");
        println!("Expected usage: cargo run <filename>");
        println!("============================================");
        panic!();
    }
    let filename = &args[1];

    let raw = fs::read_to_string(filename).expect("Something went wrong reading the file");

    let lexer = lex::Token::lexer(&raw);
    let mut token_vec: Vec<lex::Token> = lexer.collect();
    let parsed = parse::parse(&mut token_vec, 0).unwrap();
    let output = interpret::interpret(parsed);

    println!("{}", output);
}
