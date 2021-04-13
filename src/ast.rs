use std::{collections::HashMap, fmt};

pub type Env = HashMap<String, Val>;

#[derive(PartialEq, Debug, Clone)]
pub enum Ast {
    /// (defns, expr)
    ProgramNode(Vec<Ast>),
    /// (val)
    NumberNode(i64),
    /// (val)
    BoolNode(bool),
    /// (val)
    VarNode(String),
    /// (id, expr)
    LetNode(String, Box<Ast>),
    /// (cond, consq, altern)
    IfNode(Box<Ast>, Box<Ast>, Box<Ast>),
    /// (operator, operand1, operand2)
    BinOpNode(BinOp, Box<Ast>, Box<Ast>),
    /// (fun_value, arg_list)
    FunCallNode(Box<Ast>, Vec<Ast>),
    /// (param_list, body)
    LambdaNode(Vec<String>, Box<Ast>),
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum BinOp {
    Plus,
    Minus,
    Times,
    Eq,
}
#[derive(PartialEq, Debug, Clone)]
pub enum Val {
    Num(i64),
    Bool(bool),
    Lam(Vec<String>, Box<Ast>, Env),
    Unit,
}

impl fmt::Display for Val {
    // This trait requires `fmt` with this exact signature.
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Val::Num(n) => write!(f, "Num({})", n),
            Val::Bool(v) => write!(f, "Bool({})", v),
            Val::Unit => write!(f, "Unit()"),
            Val::Lam(_, _, _) => write!(f, "Lam"),
        }
    }
}
