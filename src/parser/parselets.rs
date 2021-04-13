use crate::ast::{Ast, BinOp};
use crate::lexer::lex::Token;
use crate::parser::parse;
use crate::parser::util;
use util::expect_and_consume;

use super::util::ast_op_to_token_op;

pub trait PrefixParselet {
    fn parse(&self, tokens: &mut Vec<Token>, current_token: Token)
        -> Result<Ast, util::ParseError>;
}
pub trait InfixParselet {
    fn parse(
        &self,
        tokens: &mut Vec<Token>,
        left_node: Ast,
        current_token: Token,
    ) -> Result<Ast, util::ParseError>;
}

pub trait PostfixParselet {
    fn parse(
        &self,
        tokens: &mut Vec<Token>,
        left_node: Ast,
        current_token: Token,
    ) -> Result<Ast, util::ParseError>;
}

pub struct NumberParselet {}
impl PrefixParselet for NumberParselet {
    fn parse(
        &self,
        _tokens: &mut Vec<Token>,
        current_token: Token,
    ) -> Result<Ast, util::ParseError> {
        match current_token {
            Token::Number(n) => Ok(Ast::NumberNode(n)),
            _ => panic!("Tried to use number parselet with non-number token"),
        }
    }
}

pub struct BoolParselet {}
impl PrefixParselet for BoolParselet {
    fn parse(
        &self,
        _tokens: &mut Vec<Token>,
        current_token: Token,
    ) -> Result<Ast, util::ParseError> {
        match current_token {
            Token::Bool(v) => Ok(Ast::BoolNode(v)),
            _ => panic!("Tried to use bool parselet with non-bool token"),
        }
    }
}

pub struct LambdaParselet {}
impl PrefixParselet for LambdaParselet {
    fn parse(
        &self,
        tokens: &mut Vec<Token>,
        current_token: Token,
    ) -> Result<Ast, util::ParseError> {
        match current_token {
            Token::Lambda => {
                expect_and_consume(tokens, Token::LParen)?;

                let params = parse::parse_params(tokens)?;

                expect_and_consume(tokens, Token::Colon)?;

                let body = parse::parse_expr(tokens, 0)?;

                expect_and_consume(tokens, Token::End)?;

                return Ok(Ast::LambdaNode(
                    params.iter().map(|x| x.to_string()).collect(),
                    Box::new(body),
                ));
            }
            _ => panic!("Tried to use number parselet with non-number token"),
        }
    }
}

pub struct IfParselet {}
impl PrefixParselet for IfParselet {
    fn parse(
        &self,
        tokens: &mut Vec<Token>,
        current_token: Token,
    ) -> Result<Ast, util::ParseError> {
        match current_token {
            Token::If => {
                let cond = parse::parse_expr(tokens, 0)?;

                expect_and_consume(tokens, Token::Colon)?;

                let consq = parse::parse_expr(tokens, 0)?;

                expect_and_consume(tokens, Token::Else)?;
                expect_and_consume(tokens, Token::Colon)?;

                let altern = parse::parse_expr(tokens, 0)?;

                expect_and_consume(tokens, Token::End)?;

                return Ok(Ast::IfNode(
                    Box::new(cond),
                    Box::new(consq),
                    Box::new(altern),
                ));
            }
            _ => panic!("Tried to use if parselet with non-if token"),
        }
    }
}

pub struct IdentifierParselet {}
impl PrefixParselet for IdentifierParselet {
    fn parse(
        &self,
        _tokens: &mut Vec<Token>,
        current_token: Token,
    ) -> Result<Ast, util::ParseError> {
        match current_token {
            Token::Identifier(id) => Ok(Ast::VarNode(id)),
            _ => panic!("Tried to use identifier parselet with non-id token"),
        }
    }
}

pub struct ParenthesisParselet {}
impl PrefixParselet for ParenthesisParselet {
    fn parse(
        &self,
        tokens: &mut Vec<Token>,
        _current_token: Token,
    ) -> Result<Ast, util::ParseError> {
        let expr = parse::parse_expr(tokens, 0)?;

        println!("Length before pop: {}", tokens.len());

        util::expect_and_consume(tokens, Token::RParen)?;

        println!("Length after pop: {}", tokens.len());

        return Ok(expr);
    }
}

pub struct OperatorParselet {
    operator: BinOp,
    is_left_associative: bool,
}
impl OperatorParselet {
    pub fn new(op: BinOp, is_left_associative: bool) -> OperatorParselet {
        OperatorParselet {
            operator: op,
            is_left_associative: is_left_associative,
        }
    }
}
impl InfixParselet for OperatorParselet {
    fn parse(
        &self,
        tokens: &mut Vec<Token>,
        left_node: Ast,
        _current_token: Token,
    ) -> Result<Ast, util::ParseError> {
        let my_binding_power = parse::get_binding_power(&ast_op_to_token_op(&self.operator));
        let right_node = parse::parse_expr(
            tokens,
            if self.is_left_associative {
                my_binding_power
            } else {
                my_binding_power - 1
            },
        )?;

        return Ok(Ast::BinOpNode(
            self.operator,
            Box::new(left_node),
            Box::new(right_node),
        ));
    }
}

pub struct FunCallParselet {}
impl PostfixParselet for FunCallParselet {
    fn parse(
        &self,
        tokens: &mut Vec<Token>,
        left_node: Ast,
        _current_token: Token,
    ) -> Result<Ast, util::ParseError> {
        let args = parse::parse_args(tokens)?;
        return Ok(Ast::FunCallNode(Box::new(left_node), args));
    }
}
