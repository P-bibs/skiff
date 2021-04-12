use crate::ast::{Ast, BinOp};
use crate::lexer::lex::Token;
use crate::parser::parselets::*;
use crate::parser::util;
use crate::parser::util::ParseError;

pub fn get_binding_power(op: BinOp) -> i64 {
    match op {
        BinOp::Plus => 10,
        BinOp::Times => 20,
    }
}

fn initial_map(tok: &Token) -> Option<Box<dyn InitialParselet>> {
    match *tok {
        Token::Number(_) => Some(Box::new(NumberParselet {})),
        Token::LParen => Some(Box::new(ParenthesisParselet {})),
        _ => None,
    }
}

fn consequent_map(tok: &Token) -> Option<Box<dyn ConsequentParselet>> {
    match *tok {
        Token::Plus => Some(Box::new(OperatorParselet::new(BinOp::Plus, true))),
        Token::Times => Some(Box::new(OperatorParselet::new(BinOp::Times, true))),
        _ => None,
    }
}

pub fn parse(tokens: &mut Vec<Token>, current_binding_power: i64) -> Result<Ast, ParseError> {
    // Pop the first token and find which parselet we should use
    let initial_token = tokens.pop().unwrap();
    let initial_parselet = match initial_map(&initial_token) {
        None => return Err(ParseError("Unexpected Token".to_string())),
        Some(v) => v,
    };

    let mut left_node = (*initial_parselet).parse(tokens, initial_token)?;

    loop {
        // If next token is empty then stop repeating
        let next_token = match tokens.last() {
            None => break,
            Some(v) => v,
        };

        // If we don't find a consequent parselet then stop repeating
        let consequent_parselet = match consequent_map(next_token) {
            None => break,
            Some(v) => v,
        };

        if get_binding_power(util::token_op_to_ast_op(next_token)) <= current_binding_power {
            break;
        };

        // Consume the token we peeped
        let next_token = tokens.pop().unwrap();

        left_node = consequent_parselet.parse(tokens, left_node, next_token)?;
    }

    return Ok(left_node);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_number() {
        let input: &mut Vec<Token> = &mut vec![Token::Number(1)];
        input.reverse();

        let result = parse(input, 0).unwrap();
        let expected_output = Ast::NumberNode(1);
        assert_eq!(result, expected_output);
    }

    #[test]
    fn parses_numbers_and_operators_with_precedence() {
        let input: &mut Vec<Token> = &mut vec![
            Token::Number(1),
            Token::Plus,
            Token::Number(2),
            Token::Times,
            Token::Number(3),
        ];
        input.reverse();

        let result = parse(input, 0).unwrap();
        let expected_output = Ast::BinOpNode(
            BinOp::Plus,
            Box::new(Ast::NumberNode(1)),
            Box::new(Ast::BinOpNode(
                BinOp::Times,
                Box::new(Ast::NumberNode(2)),
                Box::new(Ast::NumberNode(3)),
            )),
        );
        assert_eq!(result, expected_output);
    }

    #[test]
    fn parses_parenthesis_to_override_precedence() {
        let input: &mut Vec<Token> = &mut vec![
            Token::LParen,
            Token::Number(1),
            Token::Plus,
            Token::Number(2),
            Token::RParen,
            Token::Times,
            Token::Number(3),
        ];
        input.reverse();

        let result = parse(input, 0).unwrap();
        let expected_output = Ast::BinOpNode(
            BinOp::Times,
            Box::new(Ast::BinOpNode(
                BinOp::Plus,
                Box::new(Ast::NumberNode(1)),
                Box::new(Ast::NumberNode(2)),
            )),
            Box::new(Ast::NumberNode(3)),
        );
        assert_eq!(result, expected_output);
    }
}
