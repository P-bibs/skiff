use colored::*;
use logos::Logos;
use skiff::error_handling::pretty_print_warning;
use skiff::interpreter::interpret::StackFrame;
use skiff::static_checking::exhaustiveness::{
    check_program_exhaustiveness, ProgramExhaustivenessReport,
};
use skiff::type_inferencer::constraint_gen::find_types;
use skiff::type_inferencer::type_inference::InferenceError;
use skiff::type_inferencer::util::add_any_to_declarations;
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

    let parsed_with_anys = add_any_to_declarations(parsed.clone());

    let data_decl_table = find_types(&parsed_with_anys);

    let type_environment = match type_inference::infer_types(&parsed_with_anys, &data_decl_table) {
        Ok(t_e) => t_e,
        Err(e) => {
            // for expr in parsed_with_anys.clone() {
            //     println!("{}", expr.pretty_print());
            // }
            // println!("{:?}", parsed_with_anys);
            match e {
                InferenceError::ConstructorMismatch(t1, t2) => {
                    println!("Type mismatch: {} is not {}", t1, t2)
                }
                _ => {
                    println!("Inference error: {:?}", e);
                }
            }
            return Err(Box::new(SkiffError("Avast! Skiff execution failed")));
        }
    };

    if args.stop_after_types {
        println!("{}", "Parse tree:".bright_yellow().bold());
        for expr in parsed_with_anys {
            println!("{}", expr.pretty_print());
        }
        println!("{}", "Type environment:".bright_yellow().bold());
        println!("{:?}", type_environment);
        return Ok(());
    }

    match check_program_exhaustiveness(&parsed_with_anys, &type_environment) {
        Ok(ProgramExhaustivenessReport {
            non_exhaustive_matches,
        }) => {
            for match_loc in non_exhaustive_matches {
                pretty_print_warning(
                    "Non-exhaustive match expression",
                    match_loc.span,
                    raw.borrow(),
                    args.path.clone(),
                )
            }
        }
        Err(e) => {
            println!("{:?}", e);
            return Err(Box::new(SkiffError("Avast! Skiff execution failed")));
        }
    };

    let output = match interpret::interpret(&parsed_with_anys) {
        Ok(output) => output,
        Err(interpret::InterpError(msg, span, env, stack)) => {
            // print the error message and source location
            error_handling::pretty_print_error(msg.borrow(), span, raw.borrow(), args.path.clone());
            // print a stack trace
            StackFrame::print_stack(&stack, &args.path, raw.borrow());
            // print the environment
            println!("Environment when error occured:\n{:?}", env);

            return Err(Box::new(SkiffError("Avast! Skiff execution failed")));
        }
    };

    for val in output {
        println!("{}", val);
    }

    Ok(())
}
