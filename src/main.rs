use logos::Logos;
use skiff::{error_handling, interpreter::interpret, lexer::lex, parser::parse};
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

    let parsed = parse::parse_program(&mut token_vec)?;

    if args.stop_after_parsing {
        for expr in parsed {
            println!("{}", expr.pretty_print());
        }
        return Ok(());
    }

    let output = match interpret::interpret(&parsed) {
        Ok(output) => output,
        Err(interpret::InterpError(msg, span, env)) => {
            error_handling::pretty_print_error(msg.borrow(), span, raw.borrow(), args.path);
            println!("env: {:?}", env);
            return Err(Box::new(SkiffError("Avast! Skiff execution failed")));
        }
    };

    for val in output {
        println!("{}", val);
    }

    Ok(())
}
