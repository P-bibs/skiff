use crate::ast::{Ast, BinOp, Program};
use crate::lexer::lex::Token;
use crate::parser::parselets::*;
use crate::parser::util::ParseError;

pub fn get_binding_power(op: &Token) -> i64 {
    match op {
        Token::DoubleEq | Token::Gt | Token::Lt => 10,
        Token::Plus | Token::Minus => 20,
        Token::Times | Token::Divide => 30,
        Token::LParen => 100,
        _ => panic!("Tried to get binding power of non-op token"),
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
        _ => None,
    }
}

fn infix_map(tok: &Token) -> Option<Box<dyn InfixParselet>> {
    match *tok {
        Token::Plus => Some(Box::new(OperatorParselet::new(BinOp::Plus, true))),
        Token::Minus => Some(Box::new(OperatorParselet::new(BinOp::Minus, true))),
        Token::Times => Some(Box::new(OperatorParselet::new(BinOp::Times, true))),
        Token::Divide => Some(Box::new(OperatorParselet::new(BinOp::Divide, true))),
        Token::DoubleEq => Some(Box::new(OperatorParselet::new(BinOp::Eq, true))),
        Token::Gt => Some(Box::new(OperatorParselet::new(BinOp::Gt, true))),
        Token::Lt => Some(Box::new(OperatorParselet::new(BinOp::Lt, true))),
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
    tokens: &mut Vec<Token>,
    current_binding_power: i64,
    is_top_level: bool,
) -> Result<Ast, ParseError> {
    // Pop the first token and find which parselet we should use
    let initial_token = match tokens.pop() {
        Some(v) => v,
        None => return Err(ParseError("Unexpected end of file".to_string())),
    };

    let initial_parselet = match prefix_map(&initial_token) {
        None => {
            return Err(ParseError(
                format!("Unexpected Token: {:?}", initial_token).to_string(),
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
    use super::{parse_expr, Ast, BinOp, Token};

    #[test]
    fn parses_number() {
        let input: &mut Vec<Token> = &mut vec![Token::Number(1)];
        input.reverse();

        let result = parse_expr(input, 0, false).unwrap();
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

        let result = parse_expr(input, 0, false).unwrap();
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

        let result = parse_expr(input, 0, false).unwrap();
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

        let result = parse_expr(input, 0, false);
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

        let result = parse_expr(input, 0, false);
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
            let arg = parse_expr(tokens, 0, false)?;
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
            let expr = parse_expr(tokens, 0, false)?;
            let mut rest = parse_rest_args(tokens)?;
            rest.push(expr);
            Ok(rest)
        }
        _ => Err(ParseError("Expected comma".to_string())),
    }
}

#[cfg(test)]
mod parse_arg_tests {
    use super::{parse_args, Ast, BinOp, Token};

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

// A recursive descent parser for function parameter lists
pub fn parse_params(tokens: &mut Vec<Token>) -> Result<Vec<String>, ParseError> {
    match tokens.pop() {
        Some(Token::RParen) => Ok(vec![]),
        Some(Token::Identifier(param)) => {
            let mut rest = parse_rest_params(tokens)?;
            rest.push(param.to_string());
            rest.reverse();
            Ok(rest)
        }
        e => Err(ParseError(
            format!("Expected right paren or function param but got {:?}", e).to_string(),
        )),
    }
}

fn parse_rest_params(tokens: &mut Vec<Token>) -> Result<Vec<String>, ParseError> {
    match tokens.pop() {
        Some(Token::RParen) => Ok(vec![]),
        Some(Token::Comma) => {
            if let Some(Token::Identifier(param)) = tokens.pop() {
                let mut rest = parse_rest_params(tokens)?;
                rest.push(param);
                Ok(rest)
            } else {
                Err(ParseError("Expected identifier".to_string()))
            }
        }
        _ => Err(ParseError("Expected comma".to_string())),
    }
}

#[cfg(test)]
mod parse_params_tests {
    use super::{parse_params, ParseError, Token};

    fn test_program(
        input: &mut Vec<Token>,
        expected_output: Result<Vec<String>, ParseError>,
    ) -> () {
        input.reverse();

        let result = parse_params(input);
        assert_eq!(result, expected_output);
    }

    #[test]
    fn parses_no_params() {
        let input: &mut Vec<Token> = &mut vec![Token::RParen];

        let expected_output = Ok(vec![]);

        test_program(input, expected_output);
    }

    #[test]
    fn parses_one_param() {
        let input: &mut Vec<Token> = &mut vec![Token::Identifier("f".to_string()), Token::RParen];

        let expected_output = Ok(vec!["f".to_string()]);

        test_program(input, expected_output);
    }

    #[test]
    fn parses_two_params() {
        let input: &mut Vec<Token> = &mut vec![
            Token::Identifier("a".to_string()),
            Token::Comma,
            Token::Identifier("b".to_string()),
            Token::RParen,
        ];

        let expected_output = Ok(vec!["a".to_string(), "b".to_string()]);

        test_program(input, expected_output);
    }
}

// A recursive descent parser for the top-level program
pub fn parse_program(tokens: &mut Vec<Token>) -> Result<Program, ParseError> {
    let exprs = parse_exprs(tokens)?;
    Ok(exprs)
}

pub fn parse_exprs(tokens: &mut Vec<Token>) -> Result<Vec<Ast>, ParseError> {
    let mut exprs = vec![];
    while tokens.len() != 0 {
        let expr = parse_expr(tokens, 0, true)?;
        exprs.push(expr);
    }
    return Ok(exprs);
}

#[cfg(test)]
mod parse_program_tests {
    use super::{parse_program, Ast, BinOp, ParseError, Program, Token};

    fn test_program(input: &mut Vec<Token>, expected_output: Result<Program, ParseError>) -> () {
        input.reverse();

        let result = parse_program(input);
        assert_eq!(result, expected_output);
    }

    #[test]
    fn parses_no_exprs() {
        let input: &mut Vec<Token> = &mut vec![];

        let expected_output: Result<Program, ParseError> = Ok(vec![]);

        test_program(input, expected_output);
    }

    #[test]
    fn parses_one_simple_expr() {
        let input: &mut Vec<Token> = &mut vec![Token::Number(1)];

        let expected_output: Result<Program, ParseError> = Ok(vec![Ast::NumberNode(1)]);

        test_program(input, expected_output);
    }

    #[test]
    fn parses_two_simple_exprs() {
        let input: &mut Vec<Token> = &mut vec![Token::Number(1), Token::Number(2)];

        let expected_output: Result<Program, ParseError> =
            Ok(vec![Ast::NumberNode(1), Ast::NumberNode(2)]);

        test_program(input, expected_output);
    }

    #[test]
    fn parses_one_complex_expr() {
        let input: &mut Vec<Token> = &mut vec![
            Token::Number(1),
            Token::Plus,
            Token::Number(2),
            Token::Times,
            Token::Number(3),
        ];

        let expected_output: Result<Program, ParseError> = Ok(vec![Ast::BinOpNode(
            BinOp::Plus,
            Box::new(Ast::NumberNode(1)),
            Box::new(Ast::BinOpNode(
                BinOp::Times,
                Box::new(Ast::NumberNode(2)),
                Box::new(Ast::NumberNode(3)),
            )),
        )]);

        test_program(input, expected_output);
    }

    #[test]
    fn parses_two_complex_exprs() {
        let input: &mut Vec<Token> = &mut vec![
            Token::Number(1),
            Token::Plus,
            Token::Number(2),
            Token::Times,
            Token::Number(3),
            Token::Number(4),
            Token::Plus,
            Token::Number(5),
            Token::Times,
            Token::Number(6),
        ];

        let expected_output: Result<Program, ParseError> = Ok(vec![
            Ast::BinOpNode(
                BinOp::Plus,
                Box::new(Ast::NumberNode(1)),
                Box::new(Ast::BinOpNode(
                    BinOp::Times,
                    Box::new(Ast::NumberNode(2)),
                    Box::new(Ast::NumberNode(3)),
                )),
            ),
            Ast::BinOpNode(
                BinOp::Plus,
                Box::new(Ast::NumberNode(4)),
                Box::new(Ast::BinOpNode(
                    BinOp::Times,
                    Box::new(Ast::NumberNode(5)),
                    Box::new(Ast::NumberNode(6)),
                )),
            ),
        ]);

        test_program(input, expected_output);
    }
}
