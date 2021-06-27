use crate::ast::Type;
use crate::lexer::lex::Token;
use crate::parser::util::{expect_and_consume, ParseError};
use im::Vector;

pub fn parse_type(
    tokens: &mut Vec<(Token, std::ops::Range<usize>)>,
) -> Result<(Type, std::ops::Range<usize>), ParseError> {
    match tokens.pop() {
        Some((Token::Identifier(id), id_span)) => match tokens.last() {
            Some((Token::Lt, _)) => {
                tokens.pop();
                let (args, args_span_end) = parse_type_args(tokens)?;
                return Ok((Type::new(id, args), id_span.start..args_span_end));
            }
            Some((Token::ThinArrow, _)) => {
                tokens.pop();
                let (return_type, return_type_span) = parse_type(tokens)?;
                return Ok((
                    Type::new_func(Vector::unit(Type::new_unit(id)), return_type),
                    id_span.start..return_type_span.end,
                ));
            }
            Some(_) => return Ok((Type::new(id, Vector::new()), id_span)),
            None => Err(ParseError(
                "Ran out of tokens while parsing type".to_string(),
                None,
            )),
        },
        Some((Token::LParen, open_paren_span)) => {
            let (args, _) = parse_type_args(tokens)?;
            expect_and_consume(tokens, Token::ThinArrow)?;
            let (return_type, return_type_span) = parse_type(tokens)?;
            return Ok((
                Type::new_func(args, return_type),
                open_paren_span.start..return_type_span.end,
            ));
        }
        Some((t, span)) => Err(ParseError(
            format!("Unexpected token in type {:?}", t).to_string(),
            Some(span),
        )),
        None => Err(ParseError(
            "Ran out of tokens while parsing type".to_string(),
            None,
        )),
    }
}

fn parse_type_args(
    tokens: &mut Vec<(Token, std::ops::Range<usize>)>,
) -> Result<(Vector<Type>, usize), ParseError> {
    let mut args = Vector::new();

    loop {
        args.push_back(parse_type(tokens)?.0);
        match tokens.pop() {
            Some((Token::Comma, _)) => continue,
            Some((Token::Gt, span_end)) => return Ok((args, span_end.end)),
            Some((t, span)) => {
                return Err(ParseError(
                    format!("Unexpected token in type args {:?}", t).to_string(),
                    Some(span),
                ))
            }
            None => {
                return Err(ParseError(
                    "Ran out of tokens while parsing type args".to_string(),
                    None,
                ))
            }
        }
    }
}
