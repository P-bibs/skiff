mod common;
use common::SimpleVal;
use logos::Logos;
use skiff::type_inferencer::constraint_gen::find_types;
use skiff::type_inferencer::type_inference::{self, InferenceError};
use skiff::type_inferencer::util::add_any_to_declarations;
use skiff::{error_handling, interpreter::interpret, lexer::lex, parser::parse};
use std::collections::HashMap;
use std::fs::{self, ReadDir};
use std::{borrow::Borrow, ops::Range};

static VERBOSE: bool = true;

#[derive(PartialEq, Debug)]
struct TestError {
    message: String,
    span: Option<Range<usize>>,
    source: String,
    filename: std::path::PathBuf,
}
impl TestError {
    fn print(&self) {
        match self.span.clone() {
            Some(span) => error_handling::pretty_print_error(
                self.message.borrow(),
                span,
                self.source.borrow(),
                self.filename.clone(),
            ),
            None => println!("{}", self.message),
        }
    }
}

#[test]
pub fn diff_test() {
    let success_directory = "./tests/files/success";
    let error_directory = "./tests/files/error";

    let success_paths = fs::read_dir(success_directory).unwrap();
    let error_paths = fs::read_dir(error_directory).unwrap();

    let expected_outputs = common::get_expected_output();

    run_paths(success_paths, Some(expected_outputs));
    run_paths(error_paths, None);
}

fn run_paths(paths: ReadDir, expected_outputs: Option<HashMap<&str, Vec<SimpleVal>>>) {
    for path in paths {
        let path = path.unwrap();

        // Skip directories
        if path.file_type().unwrap().is_dir() {
            continue;
        }
        println!("Running file {:?}", &path);

        // run the file and see if it returned a result or errored
        match run_file(path.path().clone()) {
            Ok(actual_output) => {
                match &expected_outputs {
                    Some(expected_outputs) => {
                        // Try to find an expected output for this file
                        let expected_output = expected_outputs
                            .get(
                                path.file_name()
                                    .into_string()
                                    .expect("File path is invalid utf8")
                                    .as_str(),
                            )
                            .expect(&format!(
                                "Expected output not found for file {:?}",
                                path.file_name()
                            ));

                        // Assert the expected test output and actual output are equal
                        assert_eq!(
                            *expected_output,
                            actual_output,
                            "Testing file {:?}",
                            path.file_name()
                        )
                    }
                    None => {
                        // If running the file succeeded (but we were expecting an error), log the name and fail.
                        println!("{:?} expected failure but succeeded with values:", path);
                        if VERBOSE {
                            for output in actual_output {
                                println!("\t{:?}", output);
                            }
                        }
                        assert!(false);
                    }
                }
            }
            Err(e) => {
                match expected_outputs {
                    Some(_) => {
                        // If running the file errors, log the name and fail.
                        println!("Name: {:?}", path);
                        if VERBOSE {
                            e.print();
                        }
                        assert!(false);
                    }
                    None => assert!(true),
                }
            }
        }
    }
}

fn run_file<'a>(path: std::path::PathBuf) -> Result<Vec<SimpleVal>, TestError> {
    let raw = fs::read_to_string(path.clone()).expect("Something went wrong reading the file");

    let lexer = lex::Token::lexer(&raw);

    let mut token_vec: Vec<_> = lexer.spanned().collect();

    // Check for error tokens
    for (token, span) in &token_vec {
        if token == &lex::Token::Error {
            return Err(TestError {
                message: "Invalid token".to_string(),
                span: Some(span.clone()),
                source: raw,
                filename: path,
            });
        }
    }

    token_vec.reverse();

    // Parse and check for errors
    let parsed = match parse::parse_program(&mut token_vec) {
        Ok(v) => v,
        Err(e) => {
            return Err(TestError {
                message: e.0,
                span: None,
                source: raw,
                filename: path,
            })
        }
    };

    let parsed_with_anys = add_any_to_declarations(parsed);

    let data_decl_table = find_types(&parsed_with_anys);

    match type_inference::infer_types(&parsed_with_anys, &data_decl_table) {
        Err(InferenceError::ConstructorMismatch(t1, t2)) => {
            return Err(TestError {
                message: format!("Type mismatch: {} is not {}", t1, t2),
                span: None,
                source: raw,
                filename: path,
            })
        }
        Err(e) => {
            return Err(TestError {
                message: format!("Inference error {:?}", e),
                span: None,
                source: raw,
                filename: path,
            })
        }
        _ => (),
    };

    // Interpret and check for errors
    let output = match interpret::interpret(&parsed_with_anys) {
        Ok(output) => output,
        Err(interpret::InterpError(msg, span, _, _)) => {
            return Err(TestError {
                message: msg,
                span: Some(span),
                source: raw,
                filename: path,
            });
        }
    };

    // Gather output
    let mut out = vec![];
    for val in output {
        out.push(SimpleVal::new(&val))
    }

    return Ok(out);
}
