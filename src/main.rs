mod ast;
mod lexer;
mod parse;

use logos::Logos;
fn main() {
    let program = "1 + 2";

    let lexer = lexer::Token::lexer(program);
    let _ = lexer;

    let input = &mut vec![
        lexer::Token::Number(3),
        lexer::Token::Times,
        lexer::Token::Number(2),
        lexer::Token::Plus,
        lexer::Token::Number(1),
    ];
    input.reverse();
    let result = parse::parse_expr(input, None).unwrap();
    let _ = result;
}
