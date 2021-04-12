use crate::ast::BinOp;
use crate::lexer::lex::Token;

#[derive(PartialEq, Debug)]
pub struct ParseError(pub String);

pub fn ast_op_to_token_op(op: &BinOp) -> Token {
    match op {
        BinOp::Times => Token::Times,
        BinOp::Plus => Token::Plus,
    }
}

pub fn expect_and_consume(tokens: &mut Vec<Token>, expected: Token) -> Result<(), ParseError> {
    match tokens.pop() {
        None => Err(ParseError("Not tokens left to consume".to_string())),
        Some(v) if v == expected => Ok(()),
        _ => Err(ParseError("Didn't get expected token".to_string())),
    }
}
