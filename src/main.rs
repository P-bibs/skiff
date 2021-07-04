use colored::*;
use logos::Logos;
use skiff::{
    error_handling, interpreter::interpret, lexer::lex, parser::parse,
    type_inferencer::type_inference,
};
use std::{borrow::Borrow, error};
use std::{fmt, fs};
use structopt::StructOpt;

/// The interpreter for the Skiff programming language
#[derive(Debug, StructOpt)]
struct Cli {
    /// Stop after lexing
    #[structopt(short = "l", long = "lex")]
    stop_after_lexing: bool,

    /// Stop after parsing
    #[structopt(short = "p", long = "parse")]
    stop_after_parsing: bool,

    // Stop after type inference
    #[structopt(short = "t", long = "type-check")]
    stop_after_types: bool,

    /// The path to the file to interpret
    #[structopt(parse(from_os_str))]
    path: std::path::PathBuf,
}

#[derive(PartialEq, Debug)]
pub struct SkiffError<'a>(pub &'a str);
impl<'a> fmt::Display for SkiffError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}
impl<'a> error::Error for SkiffError<'a> {}

fn main() -> Result<(), Box<dyn error::Error>> {
    let args = Cli::from_args();

    let raw = fs::read_to_string(args.path.clone()).expect("Something went wrong reading the file");

    let lexer = lex::Token::lexer(&raw);

    let mut token_vec: Vec<_> = lexer.spanned().collect();

    if args.stop_after_lexing {
        println!("{:?}", token_vec);
        return Ok(());
    }

    // Check for error tokens
    for (token, span) in &token_vec {
        if token == &lex::Token::Error {
            error_handling::pretty_print_error(
                "Invalid token",
                span.clone(),
                raw.borrow(),
                args.path.clone(),
            );
            return Err(Box::new(SkiffError("Avast! Skiff execution failed")));
        }
    }

    token_vec.reverse();

    let parsed = match parse::parse_program(&mut token_vec) {
        Ok(program) => program,
        Err(skiff::parser::util::ParseError(msg, Some(span))) => {
            error_handling::pretty_print_error(msg.borrow(), span, raw.borrow(), args.path);
            return Err(Box::new(SkiffError("Avast! Skiff execution failed")));
        }
        Err(skiff::parser::util::ParseError(msg, None)) => {
            error_handling::pretty_print_error(msg.borrow(), 0..0, raw.borrow(), args.path);
            return Err(Box::new(SkiffError("Avast! Skiff execution failed")));
        }
    };

    if args.stop_after_parsing {
        for expr in parsed {
            println!("{}", expr.pretty_print());
        }
        return Ok(());
    }

    let type_environment = match type_inference::infer_types(&parsed) {
        Ok(t_e) => t_e,
        Err(e) => {
            println!("Inference error: {:?}", e);
            return Err(Box::new(SkiffError("Avast! Skiff execution failed")));
        }
    };

    if args.stop_after_types {
        println!("{}", "Parse tree:".bright_yellow().bold());
        for expr in parsed {
            println!("{}", expr.pretty_print());
        }
        println!("{}", "Type environment:".bright_yellow().bold());
        println!("{:?}", type_environment);
        return Ok(());
    }

    let output = match interpret::interpret(&parsed) {
        Ok(output) => output,
        Err(interpret::InterpError(msg, span, env, stack)) => {
            // print a stack trace
            for (i, frame) in stack.iter().enumerate() {
                println!("{}", frame.pretty_print(i, args.path.clone(), &raw));
            }
            // print the error message and source location
            error_handling::pretty_print_error(msg.borrow(), span, raw.borrow(), args.path);
            // print the environment
            println!("Environment when error occured:\n\t{:?}", env);

            return Err(Box::new(SkiffError("Avast! Skiff execution failed")));
        }
    };

    for val in output {
        println!("{}", val);
    }

    Ok(())
}
