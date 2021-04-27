use crate::ast::BinOp;
use crate::lexer::lex::Token;
use std::error;
use std::fmt;

#[derive(PartialEq, Debug)]
pub struct ParseError(pub String);
impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}
impl error::Error for ParseError {}

pub fn ast_op_to_token_op(op: &BinOp) -> Token {
    match op {
        BinOp::Times => Token::Times,
        BinOp::Plus => Token::Plus,
        BinOp::Eq => Token::DoubleEq,
        BinOp::Minus => Token::Minus,
        BinOp::Divide => Token::Divide,
        BinOp::Gt => Token::Gt,
        BinOp::Lt => Token::Lt,
    }
}

pub fn expect_and_consume(tokens: &mut Vec<Token>, expected: Token) -> Result<(), ParseError> {
    match tokens.pop() {
        None => Err(ParseError("Not tokens left to consume".to_string())),
        Some(v) if v == expected => Ok(()),
        _ => Err(ParseError("Didn't get expected token".to_string())),
    }
}
