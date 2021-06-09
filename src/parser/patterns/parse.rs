use super::parselets;
use crate::ast::Pattern;
use crate::lexer::lex::Token;
use crate::parser::util::ParseError;
use std::borrow::Borrow;

pub fn get_binding_power(op: &Token) -> i64 {
    match op {
        Token::LParen => 50,
        _ => panic!(format!("Tried to get binding power of non-op token {:?}", op).to_string()),
    }
}

fn prefix_map(tok: &Token) -> Option<Box<dyn parselets::PrefixParselet>> {
    match *tok {
        Token::Number(_) => Some(Box::new(parselets::NumberParselet {})),
        Token::Bool(_) => Some(Box::new(parselets::BoolParselet {})),
        Token::Identifier(_) => Some(Box::new(parselets::IdentifierParselet {})),
        _ => None,
    }
}

fn infix_map(tok: &Token) -> Option<Box<dyn parselets::InfixParselet>> {
    match *tok {
        Token::LParen => Some(Box::new(parselets::DataParselet {})),
        _ => None,
    }
}

pub fn parse_pattern(
    tokens: &mut Vec<(Token, std::ops::Range<usize>)>,
    current_binding_power: i64,
) -> Result<Pattern, ParseError> {
    // Pop the first token and find which parselet we should use
    let (initial_token, start_span) = match tokens.pop() {
        Some(v) => v,
        None => return Err(ParseError("Unexpected end of file".to_string(), None)),
    };

    let initial_parselet = match prefix_map(&initial_token) {
        None => {
            return Err(ParseError(
                format!("Unexpected Token: {:?}", initial_token).to_string(),
                Some(start_span),
            ))
        }
        Some(v) => v,
    };

    let mut left_node = (*initial_parselet).parse(tokens, (initial_token, start_span))?;

    loop {
        // If next token is empty then stop repeating
        let next_token = match tokens.last() {
            None => break,
            Some(v) => v,
        };

        if let Some(infix_parselet) = infix_map(next_token.0.borrow()) {
            if get_binding_power(next_token.0.borrow()) <= current_binding_power {
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

// A recursive descent parser for function argument lists
pub fn parse_pattern_args(
    tokens: &mut Vec<(Token, std::ops::Range<usize>)>,
) -> Result<Vec<Pattern>, ParseError> {
    match tokens.last() {
        Some((Token::RParen, _)) => {
            tokens.pop();
            Ok(vec![])
        }
        Some(_) => {
            let arg = parse_pattern(tokens, 0)?;
            let mut rest = parse_rest_pattern_args(tokens)?;
            rest.push(arg);
            rest.reverse();
            Ok(rest)
        }
        None => Err(ParseError(
            "Expected right paren or function arg".to_string(),
            None,
        )),
    }
}

fn parse_rest_pattern_args(
    tokens: &mut Vec<(Token, std::ops::Range<usize>)>,
) -> Result<Vec<Pattern>, ParseError> {
    match tokens.pop() {
        Some((Token::RParen, _)) => Ok(vec![]),
        Some((Token::Comma, _)) => {
            let expr = parse_pattern(tokens, 0)?;
            let mut rest = parse_rest_pattern_args(tokens)?;
            rest.push(expr);
            Ok(rest)
        }
        Some((_, span)) => Err(ParseError("Expected comma".to_string(), Some(span))),
        None => Err(ParseError(
            "Ran out of tokens while parsing pattern args".to_string(),
            None,
        )),
    }
}
