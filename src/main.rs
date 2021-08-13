use skiff::runtime::{evaluate, CliArgs};
use std::error;
use std::fmt::Write;
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

struct ConsolePrinter;
impl Write for ConsolePrinter {
    fn write_str(&mut self, s: &str) -> core::fmt::Result {
        print!("{}", s);
        Ok(())
    }
}

fn main() -> Result<(), Box<dyn error::Error>> {
    let args = CliArgs::from_args();

    let raw = fs::read_to_string(args.path.clone()).expect("Something went wrong reading the file");

    let output = evaluate(args, raw, &mut ConsolePrinter)?;

    if let Some(output) = output {
        for val in output {
            println!("{}", val);
        }
    }

    Ok(())
}
