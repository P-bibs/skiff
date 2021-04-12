mod ast;
mod lexer {
    pub mod lex;
}
mod parser {
    pub mod parse;
    pub mod parselets;
    pub mod util;
}
mod interpreter {
    pub mod interpret;
}

use interpreter::interpret;
use lexer::lex;
use logos::Logos;
use parser::{parse, util::ParseError};
use std::env;
use std::fs;

fn main() -> Result<(), ParseError> {
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
    token_vec.reverse();

    let parsed = parse::parse_program(&mut token_vec)?;
    let output = interpret::interpret(parsed);

    println!("{}", output);

    Ok(())
}
