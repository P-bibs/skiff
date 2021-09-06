mod common;
use common::SimpleVal;
use skiff::runtime::{evaluate, CliArgs, SkiffError};
use std::collections::HashMap;
use std::fmt::Write;
use std::fs::{self, ReadDir};

static VERBOSE: bool = true;

struct ConsolePrinter;
impl Write for ConsolePrinter {
    fn write_str(&mut self, s: &str) -> core::fmt::Result {
        print!("{}", s);
        Ok(())
    }
}

#[derive(PartialEq, Debug, Clone, Hash)]
struct TestError(SkiffError);

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
                        println!("Error: {:?}", e);
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

    let output = match evaluate(CliArgs::new(path), raw, &mut ConsolePrinter) {
        Ok(Some(o)) => o,
        Ok(None) => panic!("Test run aborted early"),
        Err(e) => return Err(TestError(e)),
    };

    // Gather output
    let mut out = vec![];
    for val in output {
        out.push(SimpleVal::new(&val))
    }

    return Ok(out);
}
