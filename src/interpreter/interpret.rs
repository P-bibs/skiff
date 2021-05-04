use crate::ast::{Ast, AstNode, BinOp, Env, Program, Val};
use im::HashMap;
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
    let funcs = find_functions(program)?;
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
                    Val::Lam(params, body.borrow(), HashMap::new()),
                );
            }
            _ => (),
        };
    }

    return Ok(env);
}

enum ValOrEnv<'a> {
    V(Val<'a>),
    E(Env<'a>),
}

fn interpret_top_level<'a>(
    expr: &'a Ast,
    env: Env<'a>,
    func_table: &Env<'a>,
) -> Result<ValOrEnv<'a>, InterpError> {
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
        _ => Ok(ValOrEnv::V(interpret_expr(expr, env, func_table)?)),
    }
}

fn interpret_expr<'a>(
    expr: &'a Ast,
    env: Env<'a>,
    func_table: &Env<'a>,
) -> Result<Val<'a>, InterpError> {
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
        AstNode::LambdaNode(params, body) => Ok(Val::Lam(params, body, env.clone())),
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
                    interpret_expr(body, lam_env, func_table)
                }
                _ => Err(InterpError(
                    "Function call with non-function value".to_string(),
                    expr.src_loc.span.clone(),
                )),
            }
        }
        AstNode::IfNode(cond_e, consq_e, altern_e) => {
            match interpret_expr(cond_e, env.clone(), func_table)? {
                Val::Bool(true) => interpret_expr(consq_e, env.clone(), func_table),
                Val::Bool(false) => interpret_expr(altern_e, env.clone(), func_table),
                _ => Err(InterpError(
                    "Conditional expression with non-boolean condition".to_string(),
                    expr.src_loc.span.clone(),
                )),
            }
        }
        AstNode::FunctionNode(_, _, _) => Err(InterpError(
            "Function node not at top level".to_string(),
            expr.src_loc.span.clone(),
        )),
    }
}

fn interpret_binop<'a>(
    op: BinOp,
    e1: &'a Ast,
    e2: &'a Ast,
    span: Range<usize>,
    env: Env<'a>,
    func_table: &Env<'a>,
) -> Result<Val<'a>, InterpError> {
    let v1 = interpret_expr(e1, env.clone(), func_table)?;
    let v2 = interpret_expr(e2, env.clone(), func_table)?;

    // TODO: replace with macro
    match op {
        BinOp::Plus => match (v1, v2) {
            (Val::Num(xv), Val::Num(yv)) => Ok(Val::Num(xv + yv)),
            (Val::Num(_), e) => Err(InterpError(
                format!("Bad second op to +: {}", e).to_string(),
                span,
            )),
            (e, Val::Num(_)) => Err(InterpError(
                format!("Bad first op to +: {}", e).to_string(),
                span,
            )),
            (e1, e2) => Err(InterpError(
                format!("Bad ops to +: {}\n{}", e1, e2).to_string(),
                span,
            )),
        },
        BinOp::Minus => match (v1, v2) {
            (Val::Num(xv), Val::Num(yv)) => Ok(Val::Num(xv - yv)),
            (Val::Num(_), e) => Err(InterpError(
                format!("Bad second op to -: {}", e).to_string(),
                span,
            )),
            (e, Val::Num(_)) => Err(InterpError(
                format!("Bad first op to -: {}", e).to_string(),
                span,
            )),
            (e1, e2) => Err(InterpError(
                format!("Bad ops to -: {}\n{}", e1, e2).to_string(),
                span,
            )),
        },
        BinOp::Times => match (v1, v2) {
            (Val::Num(xv), Val::Num(yv)) => Ok(Val::Num(xv * yv)),
            (Val::Num(_), e) => Err(InterpError(
                format!("Bad second op to *: {}", e).to_string(),
                span,
            )),
            (e, Val::Num(_)) => Err(InterpError(
                format!("Bad first op to *: {}", e).to_string(),
                span,
            )),
            (e1, e2) => Err(InterpError(
                format!("Bad ops to *: {}\n{}", e1, e2).to_string(),
                span,
            )),
        },
        BinOp::Divide => match (v1, v2) {
            (Val::Num(xv), Val::Num(yv)) => Ok(Val::Num(xv / yv)),
            (Val::Num(_), e) => Err(InterpError(
                format!("Bad second op to /: {}", e).to_string(),
                span,
            )),
            (e, Val::Num(_)) => Err(InterpError(
                format!("Bad first op to /: {}", e).to_string(),
                span,
            )),
            (e1, e2) => Err(InterpError(
                format!("Bad ops to /: {}\n{}", e1, e2).to_string(),
                span,
            )),
        },
        BinOp::Eq => Ok(Val::Bool(v1 == v2)),
        BinOp::Gt => match (v1, v2) {
            (Val::Num(xv), Val::Num(yv)) => Ok(Val::Bool(xv > yv)),
            (Val::Num(_), e) => Err(InterpError(
                format!("Bad second op to >: {}", e).to_string(),
                span,
            )),
            (e, Val::Num(_)) => Err(InterpError(
                format!("Bad first op to >: {}", e).to_string(),
                span,
            )),
            (e1, e2) => Err(InterpError(
                format!("Bad ops to >: {}\n{}", e1, e2).to_string(),
                span,
            )),
        },
        BinOp::Lt => match (v1, v2) {
            (Val::Num(xv), Val::Num(yv)) => Ok(Val::Bool(xv < yv)),
            (Val::Num(_), e) => Err(InterpError(
                format!("Bad second op to <: {}", e).to_string(),
                span,
            )),
            (e, Val::Num(_)) => Err(InterpError(
                format!("Bad first op to <: {}", e).to_string(),
                span,
            )),
            (e1, e2) => Err(InterpError(
                format!("Bad ops to <: {}\n{}", e1, e2).to_string(),
                span,
            )),
        },
    }
}
