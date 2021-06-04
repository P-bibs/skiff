use crate::ast::{Ast, AstNode, BinOp, Env, Pattern, Program, SrcLoc, Val};
use im::HashMap;
use std::convert::TryInto;
use std::{borrow::Borrow, error};
use std::{fmt, ops::Range};

#[derive(PartialEq, Debug)]
pub struct InterpError(pub String, pub Range<usize>);
impl fmt::Display for InterpError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}
impl error::Error for InterpError {}

pub fn interpret(program: &Program) -> Result<Vec<Val>, InterpError> {
    let data_funcs_ast = find_data_declarations(program)?;

    // Find functions and functions to make ADT literals
    let funcs = find_functions(program)?;
    let data_funcs = find_functions(&data_funcs_ast)?;
    let funcs = funcs.into_iter().chain(data_funcs).collect();

    let mut env = HashMap::new();
    let mut vals = vec![];

    for expr in program {
        match interpret_top_level(expr, env.clone(), &funcs)? {
            ValOrEnv::V(val) => vals.push(val),
            ValOrEnv::E(new_env) => env = new_env,
        }
    }

    Ok(vals)
}

fn find_functions(program: &Program) -> Result<Env, InterpError> {
    let mut env: Env = HashMap::new();

    for expr in program {
        match &expr.node {
            AstNode::FunctionNode(name, params, body) => {
                env.insert(
                    name.clone(),
                    Val::Lam(params.clone(), *body.clone(), HashMap::new()),
                );
            }
            _ => (),
        };
    }

    return Ok(env);
}

fn find_data_declarations(program: &Program) -> Result<Program, InterpError> {
    let mut program_addendum = vec![];

    for expr in program {
        match &expr {
            Ast {
                node: AstNode::DataDeclarationNode(_name, variants),
                src_loc: SrcLoc { span },
            } => {
                for (variant_name, variant_fields) in variants {
                    let func = Ast {
                        node: AstNode::FunctionNode(
                            variant_name.clone(),
                            variant_fields.iter().cloned().collect(),
                            Box::new(Ast {
                                node: AstNode::DataLiteralNode(
                                    variant_name.clone(),
                                    variant_fields
                                        .iter()
                                        .map(|s| {
                                            Box::new(Ast {
                                                node: AstNode::VarNode(s.clone()),
                                                src_loc: SrcLoc { span: span.clone() },
                                            })
                                        })
                                        .collect(),
                                ),
                                src_loc: SrcLoc { span: span.clone() },
                            }),
                        ),
                        src_loc: SrcLoc { span: span.clone() },
                    };
                    program_addendum.push(func);
                }
            }
            _ => (),
        };
    }

    return Ok(program_addendum);
}

enum ValOrEnv {
    V(Val),
    E(Env),
}

fn interpret_top_level(expr: &Ast, env: Env, func_table: &Env) -> Result<ValOrEnv, InterpError> {
    match &expr.node {
        AstNode::LetNodeTopLevel(id, binding) => {
            let val = interpret_expr(binding.borrow(), env.clone(), func_table)?;
            Ok(ValOrEnv::E(env.update(id.clone(), val)))
        }
        AstNode::LetNode(_, _, _) => Err(InterpError(
            "Found LetNode instead of LetNodeToplevel on top level".to_string(),
            expr.src_loc.span.clone(),
        )),
        AstNode::FunctionNode(_, _, _) => Ok(ValOrEnv::E(env)),
        AstNode::DataDeclarationNode(_, _) => Ok(ValOrEnv::E(env)),
        _ => Ok(ValOrEnv::V(interpret_expr(expr, env, func_table)?)),
    }
}

fn interpret_expr(expr: &Ast, env: Env, func_table: &Env) -> Result<Val, InterpError> {
    match &expr.node {
        AstNode::NumberNode(n) => Ok(Val::Num(n.clone())),
        AstNode::BoolNode(v) => Ok(Val::Bool(v.clone())),
        AstNode::VarNode(id) => match env.get(id) {
            Some(v) => Ok(v.clone()),
            None => match func_table.get(id) {
                Some(v) => Ok(v.clone()),
                None => Err(InterpError(
                    format!("Couldn't find var in environment: {}", id).to_string(),
                    expr.src_loc.span.clone(),
                )),
            },
        },
        AstNode::LetNode(id, binding, body) => interpret_expr(
            body,
            env.update(
                id.clone(),
                interpret_expr(binding, env.clone(), func_table)?,
            ),
            func_table,
        ),
        AstNode::LetNodeTopLevel(_, _) => Err(InterpError(
            "Found LetNodeTopLevel instead of LetNode in expression".to_string(),
            expr.src_loc.span.clone(),
        )),
        AstNode::BinOpNode(op, e1, e2) => {
            interpret_binop(*op, e1, e2, expr.src_loc.span.clone(), env, func_table)
        }
        AstNode::LambdaNode(params, body) => {
            Ok(Val::Lam(params.clone(), *body.clone(), env.clone()))
        }
        AstNode::FunCallNode(fun, args) => {
            let fun_value = interpret_expr(fun, env.clone(), func_table)?;
            match fun_value {
                Val::Lam(params, body, lam_env) => {
                    let mut new_env: Env = HashMap::new();
                    for (param, arg) in params.iter().zip(args) {
                        new_env.insert(
                            param.to_string(),
                            interpret_expr(arg, env.clone(), func_table)?,
                        );
                    }
                    let mut lam_env = lam_env.clone();
                    lam_env.extend(new_env);
                    interpret_expr(&body, lam_env, func_table)
                }
                _ => Err(InterpError(
                    "Function call with non-function value".to_string(),
                    expr.src_loc.span.clone(),
                )),
            }
        }
        AstNode::IfNode(conditions_and_bodies, alternate) => {
            for (condition, body) in conditions_and_bodies {
                match interpret_expr(condition, env.clone(), func_table)? {
                    Val::Bool(true) => return interpret_expr(body, env.clone(), func_table),
                    Val::Bool(false) => continue,
                    _ => {
                        return Err(InterpError(
                            "Conditional expression with non-boolean condition".to_string(),
                            expr.src_loc.span.clone(),
                        ))
                    }
                }
            }
            return interpret_expr(alternate, env.clone(), func_table);
        }
        AstNode::FunctionNode(_, _, _) => Err(InterpError(
            "Function node not at top level".to_string(),
            expr.src_loc.span.clone(),
        )),
        AstNode::DataDeclarationNode(_, _) => Err(InterpError(
            "Found DataDeclarationNode instead of LetNode in expression".to_string(),
            expr.src_loc.span.clone(),
        )),
        AstNode::DataLiteralNode(discriminant, fields) => {
            let mut values = vec![];
            for expr in fields {
                values.push(interpret_expr(expr, env.clone(), func_table)?);
            }
            return Ok(Val::Data(discriminant.clone(), values));
        }
        AstNode::MatchNode(expression_to_match, branches) => {
            for (pattern, expr) in branches {
                let val = interpret_expr(expression_to_match, env.clone(), func_table)?;
                if let Some(match_env) = match_pattern_with_value(pattern, &val) {
                    return interpret_expr(expr, env.union(match_env), func_table);
                }
            }
            Err(InterpError(
                "No branch of match expression matched value".to_string(),
                expr.src_loc.span.clone(),
            ))
        }
    }
}

fn match_pattern_with_value(pattern: &Pattern, value: &Val) -> Option<Env> {
    match pattern {
        Pattern::BoolLiteral(b) => {
            if *value == Val::Bool(*b) {
                Some(HashMap::new())
            } else {
                None
            }
        }
        Pattern::NumLiteral(n) => {
            if *value == Val::Num(*n) {
                Some(HashMap::new())
            } else {
                None
            }
        }
        Pattern::Identifier(s) => {
            if s == "_" {
                Some(HashMap::new())
            } else {
                Some(HashMap::unit(s.clone(), value.clone()))
            }
        }
        Pattern::Data(pattern_discriminant, patterns) => match value {
            Val::Data(value_discriminant, values) => {
                if pattern_discriminant == value_discriminant {
                    if patterns.len() == values.len() {
                        let mut env = HashMap::new();
                        for (pattern, value) in patterns.into_iter().zip(values) {
                            env = env.union(match_pattern_with_value(pattern, value)?);
                        }
                        Some(env)
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            _ => None,
        },
    }
}

macro_rules! interpret_binop {
    ($value1:ident, $value2:ident, $span: expr, $op:tt, $type1:ident, $type2:ident, $output_type:ident) => {
        match ($value1, $value2) {
            (Val::$type1(xv), Val::$type2(yv)) => Ok(Val::$output_type(xv $op yv)),
            (Val::$type1(_), e) => Err(InterpError(
                format!("Bad second op to {}: {}", stringify!($op), e).to_string(),
                $span,
            )),
            (e, Val::$type2(_)) => Err(InterpError(
                format!("Bad first op to {}: {}", stringify!($op), e).to_string(),
                $span,
            )),
            (e1, e2) => Err(InterpError(
                format!("Bad ops to {}: {}\n{}", stringify!($op), e1, e2).to_string(),
                $span,
            )),
        }
    };
}

fn interpret_binop(
    op: BinOp,
    e1: &Ast,
    e2: &Ast,
    span: Range<usize>,
    env: Env,
    func_table: &Env,
) -> Result<Val, InterpError> {
    let v1 = interpret_expr(e1, env.clone(), func_table)?;
    let v2 = interpret_expr(e2, env.clone(), func_table)?;

    match op {
        BinOp::Plus => interpret_binop!(v1, v2, span, +, Num, Num, Num),
        BinOp::Minus => interpret_binop!(v1, v2, span, -, Num, Num, Num),
        BinOp::Times => interpret_binop!(v1, v2, span, *, Num, Num, Num),
        BinOp::Divide => interpret_binop!(v1, v2, span, /, Num, Num, Num),
        BinOp::Modulo => interpret_binop!(v1, v2, span, %, Num, Num, Num),
        BinOp::Exp => match (v1, v2) {
            (Val::Num(xv), Val::Num(yv)) => Ok(Val::Num(xv.pow(yv.try_into().unwrap()))),
            (Val::Num(_), e) => Err(InterpError(
                format!("Bad second op to {}: {}", "**", e).to_string(),
                span,
            )),
            (e, Val::Num(_)) => Err(InterpError(
                format!("Bad first op to {}: {}", "**", e).to_string(),
                span,
            )),
            (e1, e2) => Err(InterpError(
                format!("Bad ops to {}: {}\n{}", "**", e1, e2).to_string(),
                span,
            )),
        },
        BinOp::Eq => Ok(Val::Bool(v1 == v2)),
        BinOp::Gt => interpret_binop!(v1, v2, span, >, Num, Num, Bool),
        BinOp::Lt => interpret_binop!(v1, v2, span, <, Num, Num, Bool),
        BinOp::GtEq => interpret_binop!(v1, v2, span, >=, Num, Num, Bool),
        BinOp::LtEq => interpret_binop!(v1, v2, span, <=, Num, Num, Bool),
        BinOp::LAnd => interpret_binop!(v1, v2, span, &&, Bool, Bool, Bool),
        BinOp::LOr => interpret_binop!(v1, v2, span, ||, Bool, Bool, Bool),
        BinOp::BitAnd => interpret_binop!(v1, v2, span, &, Num, Num, Num),
        BinOp::BitOr => interpret_binop!(v1, v2, span, |, Num, Num, Num),
        BinOp::BitXor => interpret_binop!(v1, v2, span, ^, Num, Num, Num),
    }
}
