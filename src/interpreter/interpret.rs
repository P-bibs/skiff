use std::collections::HashMap;

use crate::ast::{Ast, BinOp, Val};

type Env = HashMap<String, Val>;

pub fn interpret(expr: Ast) -> Val {
    interpret_env(expr, HashMap::new())
}

fn interpret_env(expr: Ast, env: Env) -> Val {
    match expr {
        Ast::NumberNode(n) => Val::Num(n),
        Ast::BinOpNode(op, e1, e2) => interpret_binop(op, e1, e2, env),
        _ => panic!("Not yet implemented in interpreter"),
    }
}

fn interpret_binop(op: BinOp, e1: Box<Ast>, e2: Box<Ast>, env: Env) -> Val {
    let op_lam = match op {
        BinOp::Plus => |x, y| x + y,
        BinOp::Times => |x, y| x * y,
    };

    let v1 = interpret_env(*e1, env.clone());
    let v2 = interpret_env(*e2, env.clone());
    match (v1, v2) {
        (Val::Num(n1), Val::Num(n2)) => Val::Num(op_lam(n1, n2)),
        _ => panic!("Got non-number operand to binop"),
    }
}
