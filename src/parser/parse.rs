use std::borrow::Borrow;

use crate::ast::{Ast, BinOp, Identifier, Program};
use crate::lexer::lex::Token;
use crate::parser::parselets::*;
use crate::parser::util::ParseError;

use super::types::parse::parse_type;

pub fn get_binding_power(op: &Token) -> i64 {
    match op {
        Token::LOr => 10,
        Token::LAnd => 20,
        Token::DoubleEq => 30,
        Token::Gt | Token::GtEq | Token::Lt | Token::LtEq => 40,
        Token::Pipe => 50,
        Token::BitXor => 60,
        Token::BitAnd => 70,
        Token::Plus | Token::Minus => 80,
        Token::Times | Token::Divide | Token::Modulo => 90,
        Token::Exp => 100,
        Token::LParen => 110,
        _ => panic!("Tried to get binding power of non-op token {:?}", op),
    }
}

fn prefix_map(tok: &Token) -> Option<Box<dyn PrefixParselet>> {
    match *tok {
        Token::Number(_) => Some(Box::new(NumberParselet {})),
        Token::Bool(_) => Some(Box::new(BoolParselet {})),
        Token::Identifier(_) => Some(Box::new(IdentifierParselet {})),
        Token::LParen => Some(Box::new(ParenthesisParselet {})),
        Token::Lambda => Some(Box::new(LambdaParselet {})),
        Token::If => Some(Box::new(IfParselet {})),
        Token::Let => Some(Box::new(LetParselet {})),
        Token::Def => Some(Box::new(FunctionParselet {})),
        Token::Data => Some(Box::new(DataParselet {})),
        Token::Match => Some(Box::new(MatchParselet {})),
        _ => None,
    }
}

fn infix_map(tok: &Token) -> Option<Box<dyn InfixParselet>> {
    match *tok {
        Token::Plus => Some(Box::new(OperatorParselet::new(BinOp::Plus, true))),
        Token::Minus => Some(Box::new(OperatorParselet::new(BinOp::Minus, true))),
        Token::Times => Some(Box::new(OperatorParselet::new(BinOp::Times, true))),
        Token::Divide => Some(Box::new(OperatorParselet::new(BinOp::Divide, true))),
        Token::Modulo => Some(Box::new(OperatorParselet::new(BinOp::Modulo, true))),
        Token::Exp => Some(Box::new(OperatorParselet::new(BinOp::Exp, false))),
        Token::DoubleEq => Some(Box::new(OperatorParselet::new(BinOp::Eq, true))),
        Token::Gt => Some(Box::new(OperatorParselet::new(BinOp::Gt, true))),
        Token::Lt => Some(Box::new(OperatorParselet::new(BinOp::Lt, true))),
        Token::GtEq => Some(Box::new(OperatorParselet::new(BinOp::GtEq, true))),
        Token::LtEq => Some(Box::new(OperatorParselet::new(BinOp::LtEq, true))),
        Token::LAnd => Some(Box::new(OperatorParselet::new(BinOp::LAnd, true))),
        Token::LOr => Some(Box::new(OperatorParselet::new(BinOp::LOr, true))),
        Token::BitAnd => Some(Box::new(OperatorParselet::new(BinOp::BitAnd, true))),
        // Token::Pipe => Some(Box::new(OperatorParselet::new(BinOp::BitOr, true))),
        Token::BitXor => Some(Box::new(OperatorParselet::new(BinOp::BitXor, true))),
        _ => None,
    }
}

fn postfix_map(tok: &Token) -> Option<Box<dyn PostfixParselet>> {
    match *tok {
        Token::LParen => Some(Box::new(FunCallParselet {})),
        _ => None,
    }
}

pub fn parse_expr(
    tokens: &mut Vec<(Token, std::ops::Range<usize>)>,
    current_binding_power: i64,
    is_top_level: bool,
) -> Result<Ast, ParseError> {
    // Pop the first token and find which parselet we should use
    let initial_token = match tokens.pop() {
        Some(v) => v,
        None => return Err(ParseError("Unexpected end of file".to_string(), None)),
    };

    let initial_parselet = match prefix_map(&initial_token.0) {
        None => {
            return Err(ParseError(
                format!("Unexpected Token: {:?}", initial_token).to_string(),
                None,
            ))
        }
        Some(v) => v,
    };

    let mut left_node = (*initial_parselet).parse(tokens, initial_token, is_top_level)?;

    loop {
        // If next token is empty then stop repeating
        let next_token = match tokens.last() {
            None => break,
            Some(v) => v,
        };

        if let Some(postfix_parselet) = postfix_map(next_token.0.borrow()) {
            if get_binding_power(next_token.0.borrow()) <= current_binding_power {
                break;
            };

            // Consume the token we peeped
            let next_token = tokens.pop().unwrap();

            left_node = postfix_parselet.parse(tokens, left_node, next_token)?;

            continue;
        }

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
pub fn parse_args(
    tokens: &mut Vec<(Token, std::ops::Range<usize>)>,
) -> Result<(Vec<Ast>, usize), ParseError> {
    match tokens.last() {
        Some((Token::RParen, span)) => {
            let end = span.end;
            tokens.pop();
            Ok((vec![], end))
        }
        Some(_) => {
            let arg = parse_expr(tokens, 0, false)?;
            let (mut rest, end) = parse_rest_args(tokens)?;
            rest.push(arg);
            rest.reverse();
            Ok((rest, end))
        }
        None => Err(ParseError(
            "Expected right paren or function arg".to_string(),
            None,
        )),
    }
}

fn parse_rest_args(
    tokens: &mut Vec<(Token, std::ops::Range<usize>)>,
) -> Result<(Vec<Ast>, usize), ParseError> {
    match tokens.pop() {
        Some((Token::RParen, span)) => Ok((vec![], span.end)),
        Some((Token::Comma, _span)) => {
            let expr = parse_expr(tokens, 0, false)?;
            let (mut rest, end) = parse_rest_args(tokens)?;
            rest.push(expr);
            Ok((rest, end))
        }
        Some((e, span)) => Err(ParseError(
            format!("Expected comma but got {:?}", e).to_string(),
            Some(span),
        )),
        None => Err(ParseError(
            "Ran out of tokens while parsing args".to_string(),
            None,
        )),
    }
}

// A recursive descent parser for function parameter lists
pub fn parse_params(
    tokens: &mut Vec<(Token, std::ops::Range<usize>)>,
) -> Result<Vec<Identifier>, ParseError> {
    match tokens.last() {
        Some((Token::RParen, _)) => {
            tokens.pop();
            Ok(vec![])
        }
        Some((_, _)) => {
            let (param, _) = parse_identifier(None, tokens)?;
            // TODO: fix this
            // consume_if_present(tokens, Token::Comma)?;
            let mut rest = parse_rest_params(tokens)?;
            rest.push(param);
            rest.reverse();
            Ok(rest)
        }
        None => Err(ParseError(
            "Ran out of tokens while parsing params".to_string(),
            None,
        )),
    }
}

fn parse_rest_params(
    tokens: &mut Vec<(Token, std::ops::Range<usize>)>,
) -> Result<Vec<Identifier>, ParseError> {
    match tokens.last() {
        Some((Token::RParen, _)) => {
            tokens.pop();
            Ok(vec![])
        }
        Some((Token::Comma, _span)) => {
            tokens.pop();
            let (param, _id) = parse_identifier(None, tokens)?;
            let mut rest = parse_rest_params(tokens)?;
            rest.push(param);
            Ok(rest)
        }
        Some((_, span)) => Err(ParseError("Expected comma".to_string(), Some(span.clone()))),
        None => Err(ParseError(
            "Ran out of tokens while parsing rest of params".to_string(),
            None,
        )),
    }
}

// A recursive descent parser for the top-level program
pub fn parse_program(
    tokens: &mut Vec<(Token, std::ops::Range<usize>)>,
) -> Result<Program, ParseError> {
    let exprs = parse_exprs(tokens)?;
    Ok(exprs)
}

pub fn parse_exprs(
    tokens: &mut Vec<(Token, std::ops::Range<usize>)>,
) -> Result<Vec<Ast>, ParseError> {
    let mut exprs = vec![];
    while tokens.len() != 0 {
        let expr = parse_expr(tokens, 0, true)?;
        exprs.push(expr);
    }
    return Ok(exprs);
}

pub fn parse_identifier(
    initial_token: Option<(Token, std::ops::Range<usize>)>,
    tokens: &mut Vec<(Token, std::ops::Range<usize>)>,
) -> Result<(Identifier, std::ops::Range<usize>), ParseError> {
    let id_token;

    match initial_token {
        Some(t) => id_token = t,
        None => match tokens.pop() {
            Some(t) => id_token = t,
            None => {
                return Err(ParseError(
                    "Ran out of tokens while parsing identifier".to_string(),
                    None,
                ))
            }
        },
    };

    return match id_token {
        (Token::Identifier(id), id_span) => match tokens.last() {
            Some((Token::Colon, _)) => {
                tokens.pop();
                let (type_decl, type_span) = parse_type(tokens)?;
                Ok((
                    Identifier::new_with_type(id, type_decl),
                    id_span.start..type_span.end,
                ))
            }
            Some(_) => Ok((Identifier::new_without_type(id), id_span)),
            None => Err(ParseError(
                "Ran out of tokens while parsing typed identifier".to_string(),
                None,
            )),
        },
        (_, span) => Err(ParseError(
            "Found non identifier token while parsing identifier".to_string(),
            Some(span),
        )),
    };
}
