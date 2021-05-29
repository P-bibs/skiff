use crate::ast::{Ast, AstNode, BinOp, SrcLoc};
use crate::lexer::lex::Token;
use crate::parser::parse::{self, parse_params};
use crate::parser::util;
use util::expect_and_consume;

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
            (Token::Number(n), span) => Ok(Ast {
                node: AstNode::NumberNode(n),
                src_loc: SrcLoc { span },
            }),
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
            (Token::Bool(v), span) => Ok(Ast {
                node: AstNode::BoolNode(v),
                src_loc: SrcLoc { span },
            }),
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
            ));
        }

        let span_start = current_token.1.start;

        let func_name = match tokens.pop() {
            Some((Token::Identifier(name), _)) => Ok(name),
            Some(_) => Err(util::ParseError(
                "Found non-identifier in function name".to_string(),
            )),
            None => Err(util::ParseError(
                "Ran out of tokens while parsing function name".to_string(),
            )),
        }?;

        expect_and_consume(tokens, Token::LParen)?;

        let params = parse::parse_params(tokens)?;

        expect_and_consume(tokens, Token::Colon)?;

        let body = parse::parse_expr(tokens, 0, false)?;

        let span_end = expect_and_consume(tokens, Token::End)?.end;

        return Ok(Ast {
            node: AstNode::FunctionNode(
                func_name,
                params.iter().map(|x| x.to_string()).collect(),
                Box::new(body),
            ),
            src_loc: SrcLoc {
                span: span_start..span_end,
            },
        });
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

        return Ok(Ast {
            node: AstNode::LambdaNode(
                params.iter().map(|x| x.to_string()).collect(),
                Box::new(body),
            ),
            src_loc: SrcLoc {
                span: span_start..span_end,
            },
        });
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
                Some((_, _)) => {
                    return Err(util::ParseError("Expected `else` or `elif`".to_string()))
                }
                None => {
                    return Err(util::ParseError(
                        "Ran out of tokens while parsing conditional".to_string(),
                    ))
                }
            }
        };

        let span_end = expect_and_consume(tokens, Token::End)?.end;

        return Ok(Ast {
            node: AstNode::IfNode(
                conditions
                    .into_iter()
                    .zip(bodies.into_iter())
                    .collect::<Vec<(Ast, Ast)>>(),
                Box::new(altern),
            ),
            src_loc: SrcLoc {
                span: span_start..span_end,
            },
        });
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

        let id = match tokens.pop() {
            Some((Token::Identifier(id), _)) => Ok(id),
            Some(_) => Err(util::ParseError(
                "Found non-identifier in let binding".to_string(),
            )),
            None => Err(util::ParseError(
                "Ran out of tokens while parsing let identifier".to_string(),
            )),
        }?;

        // TODO: make the span_end at the true end of the expression
        let span_end = expect_and_consume(tokens, Token::Eq)?.end;

        let binding = parse::parse_expr(tokens, 0, false)?;

        if is_top_level {
            return Ok(Ast {
                node: AstNode::LetNodeTopLevel(id, Box::new(binding)),
                src_loc: SrcLoc {
                    span: span_start..span_end,
                },
            });
        } else {
            let body = parse::parse_expr(tokens, 0, false)?;
            return Ok(Ast {
                node: AstNode::LetNode(id, Box::new(binding), Box::new(body)),
                src_loc: SrcLoc {
                    span: span_start..span_end,
                },
            });
        }
    }
}

pub struct IdentifierParselet {}
impl PrefixParselet for IdentifierParselet {
    fn parse(
        &self,
        _tokens: &mut Vec<(Token, std::ops::Range<usize>)>,
        current_token: (Token, std::ops::Range<usize>),
        _is_top_level: bool,
    ) -> Result<Ast, util::ParseError> {
        match current_token {
            (Token::Identifier(id), span) => Ok(Ast {
                node: AstNode::VarNode(id),
                src_loc: SrcLoc { span },
            }),
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
        _current_token: (Token, std::ops::Range<usize>),
        is_top_level: bool,
    ) -> Result<Ast, util::ParseError> {
        if !is_top_level {
            return Err(util::ParseError(
                "Data declarations can only exist at the top level".to_string(),
            ));
        }

        let (data_name, span_start) = match tokens.pop() {
            Some((Token::Identifier(id), span)) => Ok((id, span.start)),
            Some(_) => Err(util::ParseError(
                "Found non-identifier as data declaration name".to_string(),
            )),
            None => Err(util::ParseError(
                "Ran out of tokens while parsing data declaration".to_string(),
            )),
        }?;

        expect_and_consume(tokens, Token::Colon)?;
        // Initial pipe character is optional
        consume_if_present(tokens, Token::Pipe)?;

        let mut variants = vec![];

        let span_end = loop {
            let variant_name = match tokens.pop() {
                Some((Token::Identifier(id), _)) => Ok(id),
                Some(_) => Err(util::ParseError(
                    "Found non-identifier as data variant name".to_string(),
                )),
                None => Err(util::ParseError(
                    "Ran out of tokens while parsing data variant".to_string(),
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
                Some(_) => {
                    return Err(util::ParseError(
                        "Found bad token while parsing data variants".to_string(),
                    ))
                }
                None => {
                    return Err(util::ParseError(
                        "Ran out of tokens while parsing data variant".to_string(),
                    ))
                }
            }
        };

        return Ok(Ast {
            node: AstNode::DataNode(data_name, variants),
            src_loc: SrcLoc {
                span: span_start..span_end,
            },
        });
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

        return Ok(Ast {
            node: AstNode::BinOpNode(self.operator, Box::new(left_node), Box::new(right_node)),
            src_loc: SrcLoc {
                span: current_token.1,
            },
        });
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
        let args = parse::parse_args(tokens)?;
        let span = left_node.src_loc.span.clone();
        return Ok(Ast {
            node: AstNode::FunCallNode(Box::new(left_node), args),
            src_loc: SrcLoc { span },
        });
    }
}
