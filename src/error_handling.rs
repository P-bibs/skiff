use colored::*;
use std::{fmt::Write, ops::Range};

pub fn pretty_print_error(
    message: &str,
    span: Range<usize>,
    source: &str,
    filename: std::path::PathBuf,
    printer: &mut impl Write,
) -> () {
    pretty_print_diagnostic(
        message,
        span,
        source,
        filename,
        printer,
        "ERROR",
        Color::Red,
    );
}

pub fn pretty_print_warning(
    message: &str,
    span: Range<usize>,
    source: &str,
    filename: std::path::PathBuf,
    printer: &mut impl Write,
) -> () {
    pretty_print_diagnostic(
        message,
        span,
        source,
        filename,
        printer,
        "WARNING",
        Color::Yellow,
    );
}

/// Prints an error message along with the location in the source file where the error occurred
fn pretty_print_diagnostic(
    message: &str,
    span: Range<usize>,
    source: &str,
    filename: std::path::PathBuf,
    printer: &mut impl Write,
    diagnostic_type: &str,
    color: Color,
) -> () {
    // Find the start and end of the error as line/col pair.
    let (start_line, start_col) = index_to_file_position(source, span.start);
    let (end_line, end_col) = index_to_file_position(source, span.end);

    let lines: Vec<_> = source.split("\n").collect();

    // let the user know there has been an error
    let _ = writeln!(
        printer,
        "{} in {:?}: {}",
        diagnostic_type.color(color),
        filename,
        message
    );
    if span == (0..0) {
        return;
    }

    // Print the error location
    for i in start_line..(end_line + 1) {
        // print the line from the source file
        let _ = writeln!(printer, "{:4} {} {}", i + 1, "|".blue().bold(), lines[i]);

        // print the error underline (some amount of blank followed by the underline)
        let blank_size;
        let mut underline_size;
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
        if underline_size <= 0 {
            underline_size = 1;
        }
        let _ = writeln!(
            printer,
            "{:4} {} {}{}",
            "",
            "|".blue().bold(),
            " ".repeat(blank_size),
            "^".repeat(underline_size).color(color).bold()
        );
    }
}

/// Converts an index into a string into a line/column pair for that same string
pub fn index_to_file_position(source: &str, index: usize) -> (usize, usize) {
    let mut line = 0;
    let mut last_newline_index = 0;

    for (i, c) in source[0..index].chars().enumerate() {
        if c == '\n' {
            line += 1;
            last_newline_index = i;
        }
    }

    // Fix off by one errors
    let mut col = index - last_newline_index;
    if line != 0 {
        col -= 1;
    }

    return (line, col);
}

/// Adds line and column info after a filename for printing
pub fn add_position_info_to_filename(
    source: &str,
    index: usize,
    filename: &std::path::PathBuf,
) -> String {
    let (line, column) = index_to_file_position(source, index);
    format!("{}:{}:{}", filename.display(), line, column)
}
