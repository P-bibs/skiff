use crate::ast::{Ast, Op};
use crate::lexer::lex::Token;
use crate::parser::parse;

pub trait InitialParselet {
    fn parse(
        &self,
        tokens: &mut Vec<Token>,
        current_token: Token,
    ) -> Result<Ast, parse::ParseError>;
}
pub trait ConsequentParselet {
    fn parse(
        &self,
        tokens: &mut Vec<Token>,
        left_node: Ast,
        current_token: Token,
    ) -> Result<Ast, parse::ParseError>;
}

pub struct OperatorParselet {
    operator: Op,
    is_left_associative: bool,
}
impl OperatorParselet {
    pub fn new(op: Op, is_left_associative: bool) -> OperatorParselet {
        OperatorParselet {
            operator: op,
            is_left_associative: is_left_associative,
        }
    }
}
impl ConsequentParselet for OperatorParselet {
    fn parse(
        &self,
        tokens: &mut Vec<Token>,
        left_node: Ast,
        _current_token: Token,
    ) -> Result<Ast, parse::ParseError> {
        let my_binding_power = parse::get_binding_power(self.operator);
        let right_node = parse::parse(
            tokens,
            if self.is_left_associative {
                my_binding_power
            } else {
                my_binding_power - 1
            },
        )?;

        return Ok(Ast::OperatorNode(
            self.operator,
            Box::new(left_node),
            Box::new(right_node),
        ));
    }
}

pub struct NumberParselet {}
impl InitialParselet for NumberParselet {
    fn parse(
        &self,
        _tokens: &mut Vec<Token>,
        current_token: Token,
    ) -> Result<Ast, parse::ParseError> {
        match current_token {
            Token::Number(n) => Ok(Ast::NumberNode(n)),
            _ => panic!("Tried to use number parselet with non-number token"),
        }
    }
}

pub struct ParenthesisParselet {}
impl InitialParselet for ParenthesisParselet {
    fn parse(
        &self,
        _tokens: &mut Vec<Token>,
        _current_token: Token,
    ) -> Result<Ast, parse::ParseError> {
        panic!("Not yet implemented")
    }
}
