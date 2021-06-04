use super::parse;
use crate::ast::Pattern;
use crate::lexer::lex::Token;
use crate::parser::util;

pub trait PrefixParselet {
    fn parse(
        &self,
        tokens: &mut Vec<(Token, std::ops::Range<usize>)>,
        current_token: (Token, std::ops::Range<usize>),
    ) -> Result<Pattern, util::ParseError>;
}
pub trait InfixParselet {
    fn parse(
        &self,
        tokens: &mut Vec<(Token, std::ops::Range<usize>)>,
        left_node: Pattern,
        current_token: (Token, std::ops::Range<usize>),
    ) -> Result<Pattern, util::ParseError>;
}

pub struct NumberParselet {}
impl PrefixParselet for NumberParselet {
    fn parse(
        &self,
        _tokens: &mut Vec<(Token, std::ops::Range<usize>)>,
        current_token: (Token, std::ops::Range<usize>),
    ) -> Result<Pattern, util::ParseError> {
        match current_token {
            (Token::Number(n), _span) => Ok(Pattern::NumLiteral(n)),
            _ => panic!("Tried to use number parselet with non-number token"),
        }
    }
}

pub struct BoolParselet {}
impl PrefixParselet for BoolParselet {
    fn parse(
        &self,
        _tokens: &mut Vec<(Token, std::ops::Range<usize>)>,
        current_token: (Token, std::ops::Range<usize>),
    ) -> Result<Pattern, util::ParseError> {
        match current_token {
            (Token::Bool(v), span) => Ok(Pattern::BoolLiteral(v)),
            _ => panic!("Tried to use bool parselet with non-bool token"),
        }
    }
}

pub struct IdentifierParselet {}
impl PrefixParselet for IdentifierParselet {
    fn parse(
        &self,
        _tokens: &mut Vec<(Token, std::ops::Range<usize>)>,
        current_token: (Token, std::ops::Range<usize>),
    ) -> Result<Pattern, util::ParseError> {
        match current_token {
            (Token::Identifier(id), span) => Ok(Pattern::Identifier(id)),
            _ => panic!("Tried to use identifier parselet with non-id token"),
        }
    }
}

pub struct DataParselet {}
impl InfixParselet for DataParselet {
    fn parse(
        &self,
        tokens: &mut Vec<(Token, std::ops::Range<usize>)>,
        left_node: Pattern,
        _current_token: (Token, std::ops::Range<usize>),
    ) -> Result<Pattern, util::ParseError> {
        let discriminant = match left_node {
            Pattern::Identifier(discriminant) => Ok(discriminant),
            _ => Err(util::ParseError(
                "Unexpected '(' while parsing pattern".to_string(),
            )),
        }?;

        let args = parse::parse_pattern_args(tokens)?;
        return Ok(Pattern::Data(discriminant, args));
    }
}
