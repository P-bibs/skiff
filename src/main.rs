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
use parser::parse;
use std::error;
use std::fs;
use structopt::StructOpt;

/// The interpreter for the Skiff programming language
#[derive(Debug, StructOpt)]
struct Cli {
    /// Stop after parsing
    #[structopt(short = "p", long = "parse")]
    stop_after_parsing: bool,

    /// The path to the file to interpret
    #[structopt(parse(from_os_str))]
    path: std::path::PathBuf,
}

fn main() -> Result<(), Box<dyn error::Error>> {
    let args = Cli::from_args();

    let raw = fs::read_to_string(args.path).expect("Something went wrong reading the file");

    let lexer = lex::Token::lexer(&raw);
    let mut token_vec: Vec<lex::Token> = lexer.collect();
    token_vec.reverse();

    let parsed = parse::parse_program(&mut token_vec)?;

    if args.stop_after_parsing {
        print!("{:?}", parsed);
        return Ok(());
    }

    let output = interpret::interpret(parsed)?;

    println!("{}", output);

    Ok(())
}
