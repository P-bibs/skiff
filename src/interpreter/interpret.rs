use crate::ast::{Ast, BinOp, Env, Val};
use std::collections::HashMap;
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

pub fn interpret(expr: Ast) -> Result<Val, InterpError> {
    interpret_env(expr, &mut HashMap::new())
}

fn interpret_env(expr: Ast, env: &mut Env) -> Result<Val, InterpError> {
    match expr {
        Ast::ProgramNode(exprs) => {
            let mut last_val = Val::Unit;
            for expr in exprs {
                last_val = interpret_env(expr, env)?;
            }

            Ok(last_val)
        }
        Ast::NumberNode(n) => Ok(Val::Num(n)),
        Ast::VarNode(id) => match env.get(&id) {
            Some(v) => Ok(v.clone()),
            None => Err(InterpError("Couldn't find var in environmnet".to_string())),
        },
        Ast::LetNode(id, e) => {
            env.insert(id, interpret_env(*e, &mut env.clone())?);
            Ok(Val::Unit)
        }
        Ast::BinOpNode(op, e1, e2) => interpret_binop(op, e1, e2, env),
        Ast::LambdaNode(params, body) => Ok(Val::Lam(params, body, env.clone())),
        Ast::FunCallNode(fun, args) => {
            let fun_value = interpret_env(*fun, &mut env.clone())?;
            match fun_value {
                Val::Lam(params, body, lam_env) => {
                    let mut new_env: Env = HashMap::new();
                    for (param, arg) in params.iter().zip(args) {
                        new_env.insert(param.to_string(), interpret_env(arg, &mut env.clone())?);
                    }
                    let mut lam_env = lam_env.clone();
                    lam_env.extend(new_env);
                    interpret_env(*body, &mut lam_env)
                }
                _ => Err(InterpError(
                    "Function call with non-function value".to_string(),
                )),
            }
        }
    }
}

fn interpret_binop(
    op: BinOp,
    e1: Box<Ast>,
    e2: Box<Ast>,
    env: &mut Env,
) -> Result<Val, InterpError> {
    let op_lam = match op {
        BinOp::Plus => |x, y| x + y,
        BinOp::Times => |x, y| x * y,
    };

    let v1 = interpret_env(*e1, &mut env.clone())?;
    let v2 = interpret_env(*e2, &mut env.clone())?;
    match (v1, v2) {
        (Val::Num(n1), Val::Num(n2)) => Ok(Val::Num(op_lam(n1, n2))),
        _ => panic!("Got non-number operand to binop"),
    }
}
