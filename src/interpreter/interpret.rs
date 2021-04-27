use crate::ast::{Ast, BinOp, Env, Program, Val};
use im::HashMap;
use std::error;
use std::fmt;

#[derive(PartialEq, Debug)]
pub struct InterpError(pub String);
impl fmt::Display for InterpError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}
impl error::Error for InterpError {}

pub fn interpret(program: Program) -> Result<Vec<Val>, InterpError> {
    let mut env = HashMap::new();
    let mut vals = vec![];

    for expr in program {
        match interpret_top_level(expr, env.clone())? {
            ValOrEnv::V(val) => vals.push(val),
            ValOrEnv::E(new_env) => env = new_env,
        }
    }

    Ok(vals)
}

enum ValOrEnv {
    V(Val),
    E(Env),
}

fn interpret_top_level(expr: Ast, env: Env) -> Result<ValOrEnv, InterpError> {
    match expr {
        Ast::LetNodeTopLevel(id, binding) => {
            let val = interpret_env(*binding, env.clone())?;

            Ok(ValOrEnv::E(env.update(id, val)))
        }
        Ast::LetNode(_, _, _) => Err(InterpError(
            "Found LetNode instead of LetNodeToplevel on top level".to_string(),
        )),
        e => Ok(ValOrEnv::V(interpret_env(e, env)?)),
    }
}

fn interpret_env(expr: Ast, env: Env) -> Result<Val, InterpError> {
    match expr {
        Ast::NumberNode(n) => Ok(Val::Num(n)),
        Ast::BoolNode(v) => Ok(Val::Bool(v)),
        Ast::VarNode(id) => match env.get(&id) {
            Some(v) => Ok(v.clone()),
            None => Err(InterpError(
                format!("Couldn't find var in environment: {}", id).to_string(),
            )),
        },
        Ast::LetNode(id, binding, body) => {
            interpret_env(*body, env.update(id, interpret_env(*binding, env.clone())?))
        }
        Ast::LetNodeTopLevel(_, _) => Err(InterpError(
            "Found LetNodeTopLevel instead of LetNode in expression".to_string(),
        )),
        Ast::BinOpNode(op, e1, e2) => interpret_binop(op, e1, e2, env),
        Ast::LambdaNode(params, body) => Ok(Val::Lam(params, body, env.clone())),
        Ast::FunCallNode(fun, args) => {
            let fun_value = interpret_env(*fun, env.clone())?;
            match fun_value {
                Val::Lam(params, body, lam_env) => {
                    let mut new_env: Env = HashMap::new();
                    for (param, arg) in params.iter().zip(args) {
                        new_env.insert(param.to_string(), interpret_env(arg, env.clone())?);
                    }
                    let mut lam_env = lam_env.clone();
                    lam_env.extend(new_env);
                    interpret_env(*body, lam_env)
                }
                _ => Err(InterpError(
                    "Function call with non-function value".to_string(),
                )),
            }
        }
        Ast::IfNode(cond_e, consq_e, altern_e) => match interpret_env(*cond_e, env.clone())? {
            Val::Bool(true) => interpret_env(*consq_e, env.clone()),
            Val::Bool(false) => interpret_env(*altern_e, env.clone()),
            _ => Err(InterpError(
                "Conditional expression with non-boolean expression".to_string(),
            )),
        },
    }
}

fn interpret_binop(op: BinOp, e1: Box<Ast>, e2: Box<Ast>, env: Env) -> Result<Val, InterpError> {
    let op_lam = match op {
        BinOp::Plus => |x, y| match (x, y) {
            (Val::Num(xv), Val::Num(yv)) => Some(Val::Num(xv + yv)),
            _ => None,
        },
        BinOp::Minus => |x, y| match (x, y) {
            (Val::Num(xv), Val::Num(yv)) => Some(Val::Num(xv - yv)),
            _ => None,
        },
        BinOp::Times => |x, y| match (x, y) {
            (Val::Num(xv), Val::Num(yv)) => Some(Val::Num(xv * yv)),
            _ => None,
        },
        BinOp::Eq => |x, y| Some(Val::Bool(x == y)),
    };

    let v1 = interpret_env(*e1, env.clone())?;
    let v2 = interpret_env(*e2, env.clone())?;

    match op_lam(v1, v2) {
        Some(r) => Ok(r),
        None => Err(InterpError("Got incorrect types to binop".to_string())),
    }
}
