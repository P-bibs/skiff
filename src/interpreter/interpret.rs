use std::collections::HashMap;

use crate::ast::{Ast, Op, Val};

pub fn interpret(expr: Ast) -> Val {
    interpret_env(expr, HashMap::new())
}

fn interpret_env(expr: Ast, env: HashMap<String, Val>) -> Val {
    match expr {
        Ast::NumberNode(n) => Val::Num(n),
        Ast::OperatorNode(Op::Plus, e1, e2) => {
            let v1 = interpret_env(*e1, env.clone());
            let v2 = interpret_env(*e2, env.clone());
            match (v1, v2) {
                (Val::Num(n1), Val::Num(n2)) => Val::Num(n1 + n2),
                _ => panic!("Got non-number as second operand to +"),
            }
        }
        Ast::OperatorNode(Op::Times, e1, e2) => {
            let v1 = interpret_env(*e1, env.clone());
            let v2 = interpret_env(*e2, env.clone());
            match (v1, v2) {
                (Val::Num(n1), Val::Num(n2)) => Val::Num(n1 * n2),
                _ => panic!("Got non-number as second operand to *"),
            }
        }
    }
}
