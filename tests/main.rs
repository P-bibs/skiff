use logos::Logos;
use skiff::{error_handling, interpreter::interpret, lexer::lex, parser::parse};
use std::fs;
use std::{borrow::Borrow, ops::Range};

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
    let directory = "./tests/files";
    let paths = fs::read_dir(directory).unwrap();

    let verbose = true;

    for path in paths {
        let path = path.unwrap();
        if path.file_type().unwrap().is_dir() {
            continue;
        }

        let p = path.path();
        println!("Name: {}", p.display());

        match run_file(p) {
            Ok(_) => assert!(true),
            Err(e) => {
                if verbose {
                    e.print();
                }
                assert!(false);
            }
        }
    }
}

fn run_file(path: std::path::PathBuf) -> Result<(), TestError> {
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

    let _output = match interpret::interpret(&parsed) {
        Ok(output) => output,
        Err(interpret::InterpError(msg, span)) => {
            return Err(TestError {
                message: msg,
                span: Some(span),
                source: raw,
                filename: path,
            });
        }
    };

    Ok(())
}
