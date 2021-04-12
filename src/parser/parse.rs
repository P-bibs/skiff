use crate::ast::{Ast, BinOp};
use crate::lexer::lex::Token;
use crate::parser::parselets::*;
use crate::parser::util::ParseError;

pub fn get_binding_power(op: &Token) -> i64 {
    match op {
        Token::Plus => 10,
        Token::Times => 20,
        Token::LParen => 100,
        _ => panic!("Tried to get binding power of non-op token"),
    }
}

fn prefix_map(tok: &Token) -> Option<Box<dyn PrefixParselet>> {
    match *tok {
        Token::Number(_) => Some(Box::new(NumberParselet {})),
        Token::Identifier(_) => Some(Box::new(IdentifierParselet {})),
        Token::LParen => Some(Box::new(ParenthesisParselet {})),
        _ => None,
    }
}

fn infix_map(tok: &Token) -> Option<Box<dyn InfixParselet>> {
    match *tok {
        Token::Plus => Some(Box::new(OperatorParselet::new(BinOp::Plus, true))),
        Token::Times => Some(Box::new(OperatorParselet::new(BinOp::Times, true))),
        _ => None,
    }
}

fn postfix_map(tok: &Token) -> Option<Box<dyn PostfixParselet>> {
    match *tok {
        Token::LParen => Some(Box::new(FunCallParselet {})),
        _ => None,
    }
}

pub fn parse_expr(tokens: &mut Vec<Token>, current_binding_power: i64) -> Result<Ast, ParseError> {
    // Pop the first token and find which parselet we should use
    let initial_token = match tokens.pop() {
        Some(v) => v,
        None => return Err(ParseError("Unexpected end of file".to_string())),
    };

    let initial_parselet = match prefix_map(&initial_token) {
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

        if let Some(postfix_parselet) = postfix_map(next_token) {
            if get_binding_power(next_token) <= current_binding_power {
                break;
            };

            // Consume the token we peeped
            let next_token = tokens.pop().unwrap();

            left_node = postfix_parselet.parse(tokens, left_node, next_token)?;

            continue;
        }

        if let Some(infix_parselet) = infix_map(next_token) {
            if get_binding_power(next_token) <= current_binding_power {
                break;
            };

            // Consume the token we peeped
            let next_token = tokens.pop().unwrap();

            left_node = infix_parselet.parse(tokens, left_node, next_token)?;

            continue;
        }
        break;
    }

    return Ok(left_node);
}

#[cfg(test)]
mod parse_expr_tests {
    use super::*;

    #[test]
    fn parses_number() {
        let input: &mut Vec<Token> = &mut vec![Token::Number(1)];
        input.reverse();

        let result = parse_expr(input, 0).unwrap();
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

        let result = parse_expr(input, 0).unwrap();
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

        let result = parse_expr(input, 0).unwrap();
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

    #[test]
    fn parses_function_calls() {
        let input: &mut Vec<Token> = &mut vec![
            Token::Identifier("f".to_string()),
            Token::LParen,
            Token::Number(2),
            Token::Comma,
            Token::Number(3),
            Token::RParen,
        ];
        input.reverse();

        let result = parse_expr(input, 0);
        let expected_output = Ok(Ast::FunCallNode(
            Box::new(Ast::VarNode("f".to_string())),
            vec![Ast::NumberNode(2), Ast::NumberNode(3)],
        ));

        assert_eq!(result, expected_output);
    }

    #[test]
    fn parses_identifiers() {
        let input: &mut Vec<Token> = &mut vec![Token::Identifier("f".to_string())];
        input.reverse();

        let result = parse_expr(input, 0);
        let expected_output = Ok(Ast::VarNode("f".to_string()));

        assert_eq!(result, expected_output);
    }
}

// A recursive descent parser for function argument lists
pub fn parse_args(tokens: &mut Vec<Token>) -> Result<Vec<Ast>, ParseError> {
    match tokens.last() {
        Some(Token::RParen) => {
            tokens.pop();
            Ok(vec![])
        }
        Some(_) => {
            let arg = parse_expr(tokens, 0)?;
            let mut rest = parse_rest_args(tokens)?;
            rest.push(arg);
            rest.reverse();
            Ok(rest)
        }
        None => Err(ParseError(
            "Expected right paren or function arg".to_string(),
        )),
    }
}

fn parse_rest_args(tokens: &mut Vec<Token>) -> Result<Vec<Ast>, ParseError> {
    match tokens.pop() {
        Some(Token::RParen) => Ok(vec![]),
        Some(Token::Comma) => {
            let expr = parse_expr(tokens, 0)?;
            let mut rest = parse_rest_args(tokens)?;
            rest.push(expr);
            Ok(rest)
        }
        _ => Err(ParseError("Expected comma".to_string())),
    }
}

#[cfg(test)]
mod parse_arg_tests {
    use super::*;

    #[test]
    fn parses_no_args() {
        let input: &mut Vec<Token> = &mut vec![Token::RParen];
        input.reverse();

        let result = parse_args(input);
        let expected_output = Ok(vec![]);
        assert_eq!(result, expected_output);
    }

    #[test]
    fn parses_one_simple_arg() {
        let input: &mut Vec<Token> = &mut vec![Token::Number(1), Token::RParen];
        input.reverse();

        let result = parse_args(input);
        let expected_output = Ok(vec![Ast::NumberNode(1)]);
        assert_eq!(result, expected_output);
    }

    #[test]
    fn parses_two_simple_args() {
        let input: &mut Vec<Token> = &mut vec![
            Token::Number(1),
            Token::Comma,
            Token::Number(2),
            Token::RParen,
        ];
        input.reverse();

        let result = parse_args(input);
        let expected_output = Ok(vec![Ast::NumberNode(1), Ast::NumberNode(2)]);
        assert_eq!(result, expected_output);
    }

    #[test]
    fn parses_two_complex_args() {
        let input: &mut Vec<Token> = &mut vec![
            Token::Number(1),
            Token::Plus,
            Token::Number(1),
            Token::Comma,
            Token::Number(2),
            Token::Times,
            Token::Number(1),
            Token::RParen,
        ];
        input.reverse();

        let result = parse_args(input);
        let expected_output = Ok(vec![
            Ast::BinOpNode(
                BinOp::Plus,
                Box::new(Ast::NumberNode(1)),
                Box::new(Ast::NumberNode(1)),
            ),
            Ast::BinOpNode(
                BinOp::Times,
                Box::new(Ast::NumberNode(2)),
                Box::new(Ast::NumberNode(1)),
            ),
        ]);
        assert_eq!(result, expected_output);
    }
}
