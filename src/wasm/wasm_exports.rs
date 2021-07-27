use crate::{error_handling, interpreter::interpret, lexer::lex, parser, parser::parse};
use logos::Logos;
use std::borrow::Borrow;
use std::path::PathBuf;
use std::str::FromStr;
use wasm_bindgen::prelude::*;

use colored::*;
use std::ops::Range;

use super::utils::set_panic_hook;

// When the `wee_alloc` feature is enabled, use `wee_alloc` as the global
// allocator.
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[wasm_bindgen(module = "/src/wasm/skiffInterop.js")]
extern "C" {
    fn writeTermLn(s: &str) -> bool;
}

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);

    #[wasm_bindgen(js_namespace = console)]
    fn error(s: &str);
}

/// Prints an error message along with the location in the source file where the error occurred
#[wasm_bindgen]
pub fn wasm_pretty_print_error(
    message: &str,
    span_start: usize,
    span_end: usize,
    source: &str,
    filename: &str,
) -> () {
    let span = span_start..span_end;
    let filename = PathBuf::from_str(filename).unwrap();
    return wasm_pretty_print_error_helper(message, span, source, filename);
}

pub fn wasm_pretty_print_error_helper(
    message: &str,
    span: Range<usize>,
    source: &str,
    filename: PathBuf,
) -> () {
    // Find the start and end of the error as line/col pair.
    let (start_line, start_col) = error_handling::index_to_file_position(source, span.start);
    let (end_line, end_col) = error_handling::index_to_file_position(source, span.end);

    let lines: Vec<_> = source.split("\n").collect();

    // let the user know there has been an error
    writeTermLn(&format!("ERROR in {:?}: {}", filename, message));

    // Print the error location
    for i in start_line..(end_line + 1) {
        // print the line from the source file
        writeTermLn(&format!("{:4} {} {}", i + 1, "|".blue().bold(), lines[i]));

        // print the error underline (some amount of blank followed by the underline)
        let blank_size;
        let underline_size;
        if i == start_line && i == end_line {
            blank_size = start_col;
            underline_size = end_col - start_col;
        } else if i == start_line {
            blank_size = start_col;
            underline_size = lines[i].chars().count() - start_col;
        } else if i == end_line {
            blank_size = 0;
            underline_size = end_col;
        } else {
            blank_size = 0;
            underline_size = lines[i].chars().count();
        }
        writeTermLn(&format!(
            "{:4} {} {}{}",
            "",
            "|".blue().bold(),
            " ".repeat(blank_size),
            "^".repeat(underline_size).red().bold()
        ));
    }
}

#[wasm_bindgen]
pub fn evaluate(raw: &str) -> () {
    set_panic_hook();

    let filename = "main.boat";
    let lexer = lex::Token::lexer(&raw);

    let mut token_vec: Vec<_> = lexer.spanned().collect();

    // Check for error tokens
    for (token, span) in &token_vec {
        if token == &lex::Token::Error {
            wasm_pretty_print_error(
                "Invalid token",
                span.start,
                span.end,
                raw.borrow(),
                filename,
            );
            return;
        }
    }

    token_vec.reverse();

    let parsed = match parse::parse_program(&mut token_vec) {
        Ok(program) => program,
        Err(parser::util::ParseError(msg, Some(span))) => {
            wasm_pretty_print_error(msg.borrow(), span.start, span.end, raw.borrow(), filename);
            return;
        }
        Err(parser::util::ParseError(msg, None)) => {
            wasm_pretty_print_error(msg.borrow(), 0, 0, raw.borrow(), filename);
            return;
        }
    };

    let output = match interpret::interpret(&parsed) {
        Ok(output) => output,
        Err(interpret::InterpError(msg, span, env, stack)) => {
            // print a stack trace
            for (i, frame) in stack.iter().enumerate() {
                writeTermLn(&format!(
                    "{}",
                    frame.pretty_print(i, &PathBuf::from_str(filename.clone()).unwrap(), &raw)
                ));
            }
            // print the error message and source location
            wasm_pretty_print_error(msg.borrow(), span.start, span.end, raw.borrow(), filename);
            // print the environment
            writeTermLn(&format!("Environment when error occurred:\n\t{:?}", env));
            return;
        }
    };

    for val in output {
        writeTermLn(&format!("{}", val));
    }
}
