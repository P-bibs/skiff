use crate::ast::{Ast, AstNode, BinOp, SrcLoc};
use crate::lexer::lex::Token;
use crate::parser::parse::{self, parse_expr, parse_params};
use crate::parser::patterns::parse::parse_pattern;
use crate::parser::util;
use util::expect_and_consume;

use super::parse::parse_identifier;
use super::types::parse::parse_type;
use super::util::{ast_op_to_token_op, consume_if_present};

pub trait PrefixParselet {
    fn parse(
        &self,
        tokens: &mut Vec<(Token, std::ops::Range<usize>)>,
        current_token: (Token, std::ops::Range<usize>),
        is_top_level: bool,
    ) -> Result<Ast, util::ParseError>;
}
pub trait InfixParselet {
    fn parse(
        &self,
        tokens: &mut Vec<(Token, std::ops::Range<usize>)>,
        left_node: Ast,
        current_token: (Token, std::ops::Range<usize>),
    ) -> Result<Ast, util::ParseError>;
}

pub trait PostfixParselet {
    fn parse(
        &self,
        tokens: &mut Vec<(Token, std::ops::Range<usize>)>,
        left_node: Ast,
        current_token: (Token, std::ops::Range<usize>),
    ) -> Result<Ast, util::ParseError>;
}

pub struct NumberParselet {}
impl PrefixParselet for NumberParselet {
    fn parse(
        &self,
        _tokens: &mut Vec<(Token, std::ops::Range<usize>)>,
        current_token: (Token, std::ops::Range<usize>),
        _is_top_level: bool,
    ) -> Result<Ast, util::ParseError> {
        match current_token {
            (Token::Number(n), span) => Ok(Ast::new(AstNode::NumberNode(n), SrcLoc { span })),
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
        _is_top_level: bool,
    ) -> Result<Ast, util::ParseError> {
        match current_token {
            (Token::Bool(v), span) => Ok(Ast::new(AstNode::BoolNode(v), SrcLoc { span })),
            _ => panic!("Tried to use bool parselet with non-bool token"),
        }
    }
}

pub struct FunctionParselet {}
impl PrefixParselet for FunctionParselet {
    fn parse(
        &self,
        tokens: &mut Vec<(Token, std::ops::Range<usize>)>,
        current_token: (Token, std::ops::Range<usize>),
        is_top_level: bool,
    ) -> Result<Ast, util::ParseError> {
        if !is_top_level {
            return Err(util::ParseError(
                "Function definitions are only allowed at the top level".to_string(),
                Some(current_token.1),
            ));
        }

        let span_start = current_token.1.start;

        let func_name = match tokens.pop() {
            Some((Token::Identifier(name), _)) => Ok(name),
            Some((_, span)) => Err(util::ParseError(
                "Found non-identifier in function name".to_string(),
                Some(span.clone()),
            )),
            None => Err(util::ParseError(
                "Ran out of tokens while parsing function name".to_string(),
                None,
            )),
        }?;

        expect_and_consume(tokens, Token::LParen)?;

        let params = parse::parse_params(tokens)?;

        // See if there's a return type to parse
        let mut return_type = None;
        match tokens.last() {
            Some((Token::ThinArrow, _)) => {
                tokens.pop();
                return_type = Some(parse_type(tokens)?);
            }
            None => {
                return Err(util::ParseError(
                    "Ran out of tokens while parsing function name".to_string(),
                    None,
                ))
            }
            _ => {}
        };

        expect_and_consume(tokens, Token::Colon)?;

        let body = parse::parse_expr(tokens, 0, false)?;

        let span_end = expect_and_consume(tokens, Token::End)?.end;

        return Ok(Ast::new(
            AstNode::FunctionNode(
                func_name,
                params,
                return_type.map_or(None, |v| Some(v.0)),
                Box::new(body),
            ),
            SrcLoc {
                span: span_start..span_end,
            },
        ));
    }
}

pub struct LambdaParselet {}
impl PrefixParselet for LambdaParselet {
    fn parse(
        &self,
        tokens: &mut Vec<(Token, std::ops::Range<usize>)>,
        current_token: (Token, std::ops::Range<usize>),
        _is_top_level: bool,
    ) -> Result<Ast, util::ParseError> {
        let span_start = current_token.1.start;

        expect_and_consume(tokens, Token::LParen)?;

        let params = parse::parse_params(tokens)?;

        expect_and_consume(tokens, Token::Colon)?;

        let body = parse::parse_expr(tokens, 0, false)?;

        let span_end = expect_and_consume(tokens, Token::End)?.end;

        return Ok(Ast::new(
            AstNode::LambdaNode(params, Box::new(body)),
            SrcLoc {
                span: span_start..span_end,
            },
        ));
    }
}

pub struct IfParselet {}
impl PrefixParselet for IfParselet {
    fn parse(
        &self,
        tokens: &mut Vec<(Token, std::ops::Range<usize>)>,
        current_token: (Token, std::ops::Range<usize>),
        _is_top_level: bool,
    ) -> Result<Ast, util::ParseError> {
        let span_start = current_token.1.start;

        let mut conditions = vec![];
        let mut bodies = vec![];
        conditions.push(parse::parse_expr(tokens, 0, false)?);
        expect_and_consume(tokens, Token::Colon)?;
        bodies.push(parse::parse_expr(tokens, 0, false)?);

        let altern = loop {
            match tokens.pop() {
                Some((Token::Elif, _)) => {
                    conditions.push(parse::parse_expr(tokens, 0, false)?);
                    expect_and_consume(tokens, Token::Colon)?;
                    bodies.push(parse::parse_expr(tokens, 0, false)?);
                }
                Some((Token::Else, _)) => {
                    expect_and_consume(tokens, Token::Colon)?;
                    break parse::parse_expr(tokens, 0, false)?;
                }
                Some((_, span)) => {
                    return Err(util::ParseError(
                        "Expected `else` or `elif`".to_string(),
                        Some(span),
                    ))
                }
                None => {
                    return Err(util::ParseError(
                        "Ran out of tokens while parsing conditional".to_string(),
                        None,
                    ))
                }
            }
        };

        let span_end = expect_and_consume(tokens, Token::End)?.end;

        return Ok(Ast::new(
            AstNode::IfNode(
                conditions
                    .into_iter()
                    .zip(bodies.into_iter())
                    .collect::<Vec<(Ast, Ast)>>(),
                Box::new(altern),
            ),
            SrcLoc {
                span: span_start..span_end,
            },
        ));
    }
}

pub struct LetParselet {}
impl PrefixParselet for LetParselet {
    fn parse(
        &self,
        tokens: &mut Vec<(Token, std::ops::Range<usize>)>,
        current_token: (Token, std::ops::Range<usize>),
        is_top_level: bool,
    ) -> Result<Ast, util::ParseError> {
        let span_start = current_token.1.start;

        let (id, _) = parse_identifier(None, tokens)?;

        // TODO: make the span_end at the true end of the expression
        let span_end = expect_and_consume(tokens, Token::Eq)?.end;

        let binding = parse::parse_expr(tokens, 0, false)?;
        if is_top_level {
            return Ok(Ast::new(
                AstNode::LetNodeTopLevel(id, Box::new(binding)),
                SrcLoc {
                    span: span_start..span_end,
                },
            ));
        } else {
            let body = parse::parse_expr(tokens, 0, false)?;
            return Ok(Ast::new(
                AstNode::LetNode(id, Box::new(binding), Box::new(body)),
                SrcLoc {
                    span: span_start..span_end,
                },
            ));
        }
    }
}

pub struct IdentifierParselet {}
impl PrefixParselet for IdentifierParselet {
    fn parse(
        &self,
        tokens: &mut Vec<(Token, std::ops::Range<usize>)>,
        current_token: (Token, std::ops::Range<usize>),
        _is_top_level: bool,
    ) -> Result<Ast, util::ParseError> {
        match current_token {
            (Token::Identifier(id), span) => Ok(Ast::new(AstNode::VarNode(id), SrcLoc { span })),
            _ => panic!("Tried to use identifier parselet with non-id token"),
        }
    }
}

pub struct ParenthesisParselet {}
impl PrefixParselet for ParenthesisParselet {
    fn parse(
        &self,
        tokens: &mut Vec<(Token, std::ops::Range<usize>)>,
        _current_token: (Token, std::ops::Range<usize>),
        _is_top_level: bool,
    ) -> Result<Ast, util::ParseError> {
        let expr = parse::parse_expr(tokens, 0, false)?;

        expect_and_consume(tokens, Token::RParen)?;

        return Ok(expr);
    }
}

pub struct DataParselet {}
impl PrefixParselet for DataParselet {
    fn parse(
        &self,
        tokens: &mut Vec<(Token, std::ops::Range<usize>)>,
        current_token: (Token, std::ops::Range<usize>),
        is_top_level: bool,
    ) -> Result<Ast, util::ParseError> {
        if !is_top_level {
            return Err(util::ParseError(
                "Data declarations can only exist at the top level".to_string(),
                Some(current_token.1),
            ));
        }

        let (data_name, span_start) = match tokens.pop() {
            Some((Token::Identifier(id), span)) => Ok((id, span.start)),
            Some((_, span)) => Err(util::ParseError(
                "Found non-identifier as data declaration name".to_string(),
                Some(span),
            )),
            None => Err(util::ParseError(
                "Ran out of tokens while parsing data declaration".to_string(),
                None,
            )),
        }?;

        expect_and_consume(tokens, Token::Colon)?;
        // Initial pipe character is optional
        consume_if_present(tokens, Token::Pipe)?;

        let mut variants = vec![];

        let span_end = loop {
            let variant_name = match tokens.pop() {
                Some((Token::Identifier(id), _)) => Ok(id),
                Some((_, span)) => Err(util::ParseError(
                    "Found non-identifier as data variant name".to_string(),
                    Some(span),
                )),
                None => Err(util::ParseError(
                    "Ran out of tokens while parsing data variant".to_string(),
                    None,
                )),
            }?;

            // parse variant body
            expect_and_consume(tokens, Token::LParen)?;
            let field_names = parse_params(tokens)?;

            variants.push((variant_name, field_names));

            // Determine whether we have another variant to parse or if this is the end
            match tokens.pop() {
                Some((Token::Pipe, _)) => continue,
                Some((Token::End, span)) => break span.end,
                Some((_, span)) => {
                    return Err(util::ParseError(
                        "Found bad token while parsing data variants".to_string(),
                        Some(span),
                    ))
                }
                None => {
                    return Err(util::ParseError(
                        "Ran out of tokens while parsing data variant".to_string(),
                        None,
                    ))
                }
            }
        };

        return Ok(Ast::new(
            AstNode::DataDeclarationNode(data_name, variants),
            SrcLoc {
                span: span_start..span_end,
            },
        ));
    }
}

pub struct MatchParselet {}
impl PrefixParselet for MatchParselet {
    fn parse(
        &self,
        tokens: &mut Vec<(Token, std::ops::Range<usize>)>,
        current_token: (Token, std::ops::Range<usize>),
        _is_top_level: bool,
    ) -> Result<Ast, util::ParseError> {
        let span_start = current_token.1.end;
        let expression_to_match = parse_expr(tokens, 0, false)?;

        expect_and_consume(tokens, Token::Colon)?;
        // Initial pipe character is optional
        expect_and_consume(tokens, Token::Pipe)?;

        let mut branches = vec![];

        let span_end = loop {
            let branch_pattern = parse_pattern(tokens, 0)?;

            // parse variant body
            expect_and_consume(tokens, Token::FatArrow)?;
            let branch_body = parse_expr(tokens, 0, false)?;

            branches.push((branch_pattern, branch_body));

            // Determine whether we have another variant to parse or if this is the end
            match tokens.pop() {
                Some((Token::Pipe, _)) => continue,
                Some((Token::End, span)) => break span.end,
                Some((token, span)) => {
                    return Err(util::ParseError(
                        format!(
                            "Found bad token while parsing match expression: {:?}",
                            token
                        )
                        .to_string(),
                        Some(span),
                    ))
                }
                None => {
                    return Err(util::ParseError(
                        "Ran out of tokens while parsing match expression".to_string(),
                        None,
                    ))
                }
            }
        };

        return Ok(Ast::new(
            AstNode::MatchNode(Box::new(expression_to_match), branches),
            SrcLoc {
                span: span_start..span_end,
            },
        ));
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
        tokens: &mut Vec<(Token, std::ops::Range<usize>)>,
        left_node: Ast,
        current_token: (Token, std::ops::Range<usize>),
    ) -> Result<Ast, util::ParseError> {
        let my_binding_power = parse::get_binding_power(&ast_op_to_token_op(&self.operator));
        let right_node = parse::parse_expr(
            tokens,
            if self.is_left_associative {
                my_binding_power
            } else {
                my_binding_power - 1
            },
            false,
        )?;

        return Ok(Ast::new(
            AstNode::BinOpNode(self.operator, Box::new(left_node), Box::new(right_node)),
            SrcLoc {
                span: current_token.1,
            },
        ));
    }
}

pub struct FunCallParselet {}
impl PostfixParselet for FunCallParselet {
    fn parse(
        &self,
        tokens: &mut Vec<(Token, std::ops::Range<usize>)>,
        left_node: Ast,
        _current_token: (Token, std::ops::Range<usize>),
    ) -> Result<Ast, util::ParseError> {
        let (args, span_end) = parse::parse_args(tokens)?;
        let span_start = left_node.src_loc.span.start;
        return Ok(Ast::new(
            AstNode::FunCallNode(Box::new(left_node), args),
            SrcLoc {
                span: span_start..span_end,
            },
        ));
    }
}
