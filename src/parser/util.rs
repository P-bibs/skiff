use crate::ast::BinOp;
use crate::lexer::lex::Token;
use std::fmt;
use std::{error, ops::Range};

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

pub fn expect_and_consume(
    tokens: &mut Vec<(Token, std::ops::Range<usize>)>,
    expected: Token,
) -> Result<Range<usize>, ParseError> {
    match tokens.pop() {
        None => Err(ParseError("No tokens left to consume".to_string())),
        Some((v, span)) if v == expected => Ok(span),
        Some((_, span)) => Err(ParseError(
            format!("Didn't get expected token {:?} at {:?}", expected, span).to_string(),
        )),
    }
}

pub fn consume_if_present(
    tokens: &mut Vec<(Token, std::ops::Range<usize>)>,
    expected: Token,
) -> Result<Option<(Token, std::ops::Range<usize>)>, ParseError> {
    match tokens.last() {
        None => Err(ParseError("No tokens left to consume".to_string())),
        Some((v, _)) if v == &expected => Ok(Some(tokens.pop().unwrap())),
        _ => Ok(None),
    }
}
