use crate::lexer::Token;
use std::convert::TryFrom;

#[derive(PartialEq, Debug)]
pub enum Ast {
    NumberNode(i64),
    OperatorNode(Op, Box<Ast>, Box<Ast>),
}
#[derive(PartialEq, Debug, Clone, Copy)]
pub enum Op {
    Plus,
    Times,
}
#[derive(PartialEq, Debug)]
pub struct ParseError(String);

struct OperatorParselet {
    operator: Op,
}

impl OperatorParselet {
    fn parse(self, tokens: &mut Vec<Token>, current_token: Token, left_node: Ast) -> Ast {
        let my_binding_power = get_binding_power(self.operator);
    }
}

fn get_binding_power(op: Op) -> u64 {
    match op {
        Op::Plus => 10,
        Op::Times => 20,
    }
}

fn greater_binding_power(next_op: Op, last_op: Option<Op>) -> bool {
    match last_op {
        None => true,
        Some(last_op) => get_binding_power(next_op) > get_binding_power(last_op),
    }
}

fn token_op_to_ast_op(op: &Token) -> Option<Op> {
    match op {
        Token::Plus => Some(Op::Plus),
        Token::Times => Some(Op::Times),
        _ => None,
    }
}

pub fn parse_expr(tokens: &mut Vec<Token>, last_operator: Option<Op>) -> Result<Ast, ParseError> {
    let first_token = tokens.pop().unwrap();
    let mut left_node;

    match first_token {
        Token::Number(n) => left_node = Ast::NumberNode(n),
        _ => return Err(ParseError("Expected a number".to_string())),
    }

    loop {
        // If next token is empty then stop repeating
        let next_token = match tokens.last() {
            None => break,
            Some(v) => v,
        };

        let next_token = token_op_to_ast_op(next_token).expect("Parsed token that wasn't op");

        if greater_binding_power(next_token, last_operator) {
            tokens.pop();
            let right_node = parse_expr(tokens, Some(next_token)).unwrap();
            left_node = Ast::OperatorNode(next_token, Box::new(left_node), Box::new(right_node));
        } else {
            return Ok(left_node);
        }
    }

    return Ok(left_node);
}

#[cfg(test)]
mod tests {
    use super::*;

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

        let result = parse_expr(input, None).unwrap();
        let expected_output = Ast::OperatorNode(
            Op::Plus,
            Box::new(Ast::OperatorNode(
                Op::Times,
                Box::new(Ast::NumberNode(3)),
                Box::new(Ast::NumberNode(2)),
            )),
            Box::new(Ast::NumberNode(1)),
        );
        assert_eq!(result, expected_output);
    }
}
