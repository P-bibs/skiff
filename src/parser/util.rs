use crate::ast::Op;
use crate::lexer::lex::Token;

#[derive(PartialEq, Debug)]
pub struct ParseError(pub String);

pub fn token_op_to_ast_op(tok: &Token) -> Op {
    match tok {
        Token::Times => Op::Times,
        Token::Plus => Op::Plus,
        _ => panic!("This token does not correspond to an operator"),
    }
}

pub fn expect_and_consume(tokens: &mut Vec<Token>, expected: Token) -> Result<(), ParseError> {
    match tokens.pop() {
        None => Err(ParseError("Not tokens left to consume".to_string())),
        Some(v) if v == expected => Ok(()),
        _ => Err(ParseError("Didn't get expected token".to_string())),
    }
}
