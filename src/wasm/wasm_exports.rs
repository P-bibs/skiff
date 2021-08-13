use crate::error_handling;
use crate::runtime::CliArgs;
use std::fmt::Write;
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

pub struct WasmPrinter {
    chars: Vec<char>,
}
impl WasmPrinter {
    pub fn new() -> WasmPrinter {
        WasmPrinter { chars: Vec::new() }
    }
}
impl Write for WasmPrinter {
    fn write_char(&mut self, c: char) -> std::fmt::Result {
        if c == '\n' {
            log("Flushing");
            writeTermLn(&self.chars.iter().cloned().collect::<String>());
            self.chars.clear();
        } else {
            self.chars.push(c);
        }

        Ok(())
    }
    fn write_str(&mut self, s: &str) -> core::fmt::Result {
        // log(s);
        // log(&format!("{:?}", s));
        log(&format!("writing {} chars", s.len()));
        for s in s.chars() {
            self.write_char(s);
        }

        Ok(())
    }
}

/// Prints an error message along with the location in the source file where the error occurred
// #[wasm_bindgen]
// pub fn wasm_pretty_print_error(
//     message: &str,
//     span_start: usize,
//     span_end: usize,
//     source: &str,
//     filename: &str,
// ) -> () {
//     let span = span_start..span_end;
//     let filename = PathBuf::from_str(filename).unwrap();
//     return wasm_pretty_print_error_helper(message, span, source, filename);
// }

// pub fn wasm_pretty_print_error_helper(
//     message: &str,
//     span: Range<usize>,
//     source: &str,
//     filename: PathBuf,
// ) -> () {
//     // Find the start and end of the error as line/col pair.
//     let (start_line, start_col) = error_handling::index_to_file_position(source, span.start);
//     let (end_line, end_col) = error_handling::index_to_file_position(source, span.end);

//     let lines: Vec<_> = source.split("\n").collect();

//     // let the user know there has been an error
//     writeTermLn(&format!("ERROR in {:?}: {}", filename, message));

//     // Print the error location
//     for i in start_line..(end_line + 1) {
//         // print the line from the source file
//         writeTermLn(&format!("{:4} {} {}", i + 1, "|".blue().bold(), lines[i]));

//         // print the error underline (some amount of blank followed by the underline)
//         let blank_size;
//         let underline_size;
//         if i == start_line && i == end_line {
//             blank_size = start_col;
//             underline_size = end_col - start_col;
//         } else if i == start_line {
//             blank_size = start_col;
//             underline_size = lines[i].chars().count() - start_col;
//         } else if i == end_line {
//             blank_size = 0;
//             underline_size = end_col;
//         } else {
//             blank_size = 0;
//             underline_size = lines[i].chars().count();
//         }
//         writeTermLn(&format!(
//             "{:4} {} {}{}",
//             "",
//             "|".blue().bold(),
//             " ".repeat(blank_size),
//             "^".repeat(underline_size).red().bold()
//         ));
//     }
// }

#[wasm_bindgen]
pub fn evaluate(raw: String) -> () {
    colored::control::set_override(true);
    set_panic_hook();

    let args = CliArgs::new(std::path::PathBuf::from("main.boat"));

    let output = crate::runtime::evaluate(args, raw, &mut WasmPrinter::new());

    if let Ok(Some(e)) = output {
        for val in e {
            writeTermLn(&format!("{}", val));
        }
    } else {
        // writeTerm("An internal execution error occured. See console for output\n");
        error(&format!("{:?}", output));
    }
}
