use colored::*;
use std::ops::Range;

pub fn pretty_print_error(
    message: &str,
    span: Range<usize>,
    source: &str,
    filename: std::path::PathBuf,
) -> () {
    let line_range = find_lines_of_chars(source, span);

    let lines: Vec<_> = source.split("\n").collect();

    println!("ERROR in {:?}: {}", filename, message);
    for i in line_range {
        println!("{:4} {} {}", i + 1, "|".blue().bold(), lines[i]);
    }
}

/// Returns a span that represents what lines the given span occurs on
fn find_lines_of_chars(source: &str, span: Range<usize>) -> Range<usize> {
    let mut start_line = 0;

    for c in source[0..span.start].chars() {
        if c == '\n' {
            start_line += 1;
        }
    }

    let mut end_line = start_line;

    for c in source[span.start..span.end].chars() {
        if c == '\n' {
            end_line += 1;
        }
    }

    return start_line..(end_line + 1);
}
