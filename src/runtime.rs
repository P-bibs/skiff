use crate::ast::Val;
use crate::error_handling::pretty_print_warning;
use crate::interpreter::interpret::{InterpError, StackFrame};
use crate::parser::util::ParseError;
use crate::static_checking::exhaustiveness::{
    check_program_exhaustiveness, ExhaustivenessError, ProgramExhaustivenessReport,
};
use crate::type_inferencer::constraint_gen::find_types;
use crate::type_inferencer::type_inference::InferenceError;
use crate::type_inferencer::util::add_any_to_declarations;
use crate::{
    error_handling, interpreter::interpret, lexer::lex, parser::parse,
    type_inferencer::type_inference,
};
use colored::*;
use im::HashMap;
use logos::Logos;
use std::fmt;
use std::fmt::Write;
use std::{borrow::Borrow, error};
use structopt::StructOpt;

/// The interpreter for the Skiff programming language
#[derive(Debug, StructOpt)]
pub struct CliArgs {
    /// Stop after lexing
    #[structopt(short = "l", long = "lex")]
    pub stop_after_lexing: bool,

    /// Stop after parsing
    #[structopt(short = "p", long = "parse")]
    pub stop_after_parsing: bool,

    // Stop after type inference
    #[structopt(short = "t", long = "type-check")]
    pub stop_after_types: bool,

    /// The path to the file to interpret
    #[structopt(parse(from_os_str))]
    pub path: std::path::PathBuf,
}
impl CliArgs {
    pub fn new(path: std::path::PathBuf) -> CliArgs {
        CliArgs {
            stop_after_lexing: false,
            stop_after_parsing: false,
            stop_after_types: false,
            path,
        }
    }
}

#[derive(PartialEq, Debug, Clone, Hash)]
pub enum SkiffError {
    Lex(),
    Parse(ParseError),
    Inference(InferenceError),
    Exhaustiveness(ExhaustivenessError),
    Interpret(InterpError),
}

impl<'a> fmt::Display for SkiffError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}
impl<'a> error::Error for SkiffError {}

pub fn evaluate(
    args: CliArgs,
    raw: String,
    printer: &mut impl Write,
) -> Result<Option<Vec<Val>>, SkiffError> {
    let lexer = lex::Token::lexer(&raw);

    let mut token_vec: Vec<_> = lexer.spanned().collect();

    if args.stop_after_lexing {
        let _ = writeln!(printer, "{:?}", token_vec);
        return Ok(None);
    }

    // Check for error tokens
    for (token, span) in &token_vec {
        if token == &lex::Token::Error {
            error_handling::pretty_print_error(
                "Invalid token",
                span.clone(),
                raw.borrow(),
                args.path.clone(),
                printer,
            );
            return Err(SkiffError::Lex());
        }
    }

    token_vec.reverse();

    let parsed = match parse::parse_program(&mut token_vec) {
        Ok(program) => program,
        Err(parse_error) => {
            let ParseError(message, span) = parse_error.clone();
            error_handling::pretty_print_error(
                &message,
                span.unwrap_or(0..0),
                raw.borrow(),
                args.path,
                printer,
            );
            return Err(SkiffError::Parse(parse_error));
        }
    };

    if args.stop_after_parsing {
        for expr in parsed {
            let _ = writeln!(printer, "{}", expr.pretty_print());
        }
        return Ok(None);
    }

    let parsed_with_anys = add_any_to_declarations(parsed.clone());

    let data_decl_table = find_types(&parsed_with_anys);

    let type_environment = match type_inference::infer_types(&parsed_with_anys, &data_decl_table) {
        Ok(t_e) => t_e,
        Err(e) => {
            match e.clone() {
                InferenceError::ConstructorMismatch(t1, t2) => {
                    pretty_print_warning(
                        &format!("Type mismatch: {} is not {}", t1, t2),
                        0..0,
                        raw.borrow(),
                        args.path.clone(),
                        printer,
                    );
                }
                _ => {
                    pretty_print_warning(
                        &format!("Inference error: {:?}", e),
                        0..0,
                        raw.borrow(),
                        args.path.clone(),
                        printer,
                    );
                }
            };
            HashMap::new()
        }
    };

    if args.stop_after_types {
        let _ = writeln!(printer, "{}", "Parse tree:".bright_yellow().bold());
        for expr in parsed_with_anys {
            let _ = writeln!(printer, "{}", expr.pretty_print());
        }
        let _ = writeln!(printer, "{}", "Type environment:".bright_yellow().bold());
        let _ = writeln!(printer, "{:?}", type_environment);
        return Ok(None);
    }

    // Only perform exhaustiveness checking if the type environment was constructed
    if type_environment.len() != 0 {
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
                        printer,
                    )
                }
            }
            Err(ExhaustivenessError::CantMatchAny()) => {}
            Err(e) => {
                let _ = writeln!(printer, "{:?}", e);
                return Err(SkiffError::Exhaustiveness(e));
            }
        };
    }

    let output = match interpret::interpret(&parsed_with_anys) {
        Ok(output) => output,
        Err(interp_error) => {
            let InterpError(msg, span, env, stack) = interp_error.clone();
            // print the error message and source location
            error_handling::pretty_print_error(
                msg.borrow(),
                span,
                raw.borrow(),
                args.path.clone(),
                printer,
            );
            // print a stack trace
            StackFrame::print_stack(&stack, &args.path, raw.borrow(), printer);
            // print the environment
            let _ = writeln!(printer, "Environment when error occured:\n{:?}", env);

            return Err(SkiffError::Interpret(interp_error));
        }
    };

    return Ok(Some(output));
}
