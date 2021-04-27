use im::HashMap;
use std::{fmt, usize};

pub type Env = HashMap<String, Val>;
pub type Program = Vec<Ast>;
#[derive(PartialEq, Debug, Clone)]
pub enum Ast {
    /// (val)
    NumberNode(i64),
    /// (val)
    BoolNode(bool),
    /// (val)
    VarNode(String),
    /// (id, expr)
    LetNodeTopLevel(String, Box<Ast>),
    /// (id, expr, body)
    LetNode(String, Box<Ast>, Box<Ast>),
    /// (cond, consq, altern)
    IfNode(Box<Ast>, Box<Ast>, Box<Ast>),
    /// (operator, operand1, operand2)
    BinOpNode(BinOp, Box<Ast>, Box<Ast>),
    /// (fun_value, arg_list)
    FunCallNode(Box<Ast>, Vec<Ast>),
    /// (param_list, body)
    LambdaNode(Vec<String>, Box<Ast>),
}
impl Ast {
    pub fn pretty_print(&self) -> String {
        self.pretty_print_helper(0)
    }

    fn pretty_print_helper(&self, indent_level: usize) -> String {
        let content = match &self {
            Ast::NumberNode(e) => format!("NumberNode({})", e),
            Ast::BoolNode(e) => format!("BoolNode({})", e),
            Ast::VarNode(e) => format!("VarNode({})", e),
            Ast::LetNodeTopLevel(id, binding) => format!(
                "LetNodeTopLevel(id: {}, binding: {})",
                id,
                binding.pretty_print_helper(indent_level + 1)
            ),
            Ast::LetNode(id, binding, body) => format!(
                "LetNode(id: {}, binding: {}, body: {})",
                id,
                binding.pretty_print_helper(indent_level + 1),
                body.pretty_print_helper(indent_level + 1)
            ),
            Ast::IfNode(cond, consq, altern) => format!(
                "IfNode(cond: {}, consq: {}, altern: {})",
                cond.pretty_print_helper(indent_level + 1),
                consq.pretty_print_helper(indent_level + 1),
                altern.pretty_print_helper(indent_level + 1)
            ),
            Ast::BinOpNode(op, e1, e2) => format!(
                "BinOpNode(op: {:?}, e1: {}, e2: {}",
                op,
                e1.pretty_print_helper(indent_level + 1),
                e2.pretty_print_helper(indent_level + 1)
            ),
            Ast::FunCallNode(fun, args) => format!(
                "FunCallNode(fun: {}, args: {})",
                fun.pretty_print_helper(indent_level + 1),
                args.iter()
                    .map(|x| x.pretty_print_helper(indent_level + 1))
                    .collect::<Vec<String>>()
                    .join(",\n")
            ),
            Ast::LambdaNode(params, body) => format!(
                "LambdaNode(params: {}, body: {})",
                params.join(", \n"),
                body.pretty_print_helper(indent_level + 1)
            ),
        };
        format!("\n{}{}", "\t".repeat(indent_level), content)
    }
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
}

impl fmt::Display for Val {
    // This trait requires `fmt` with this exact signature.
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Val::Num(n) => write!(f, "Num({})", n),
            Val::Bool(v) => write!(f, "Bool({})", v),
            Val::Lam(_, _, _) => write!(f, "Lam"),
        }
    }
}
