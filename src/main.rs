mod lexer;
use logos::Logos;
fn main() {
    let program = "1 + 2";

    let mut lexer = lexer::Token::lexer(program);

    lexer.next();
}
