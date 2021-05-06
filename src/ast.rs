use im::HashMap;
use std::{fmt, ops::Range, usize};

pub type Env<'a> = HashMap<String, Val<'a>>;
pub type Program = Vec<Ast>;
#[derive(PartialEq, Debug, Clone)]
pub enum AstNode {
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
    /// (function_name, param_list, body)
    FunctionNode(String, Vec<String>, Box<Ast>),
}

#[derive(PartialEq, Debug, Clone)]
pub struct SrcLoc {
    pub span: Range<usize>,
}
#[derive(PartialEq, Debug, Clone)]
pub struct Ast {
    pub node: AstNode,
    pub src_loc: SrcLoc,
}

impl Ast {
    pub fn pretty_print(&self) -> String {
        self.pretty_print_helper(0)
    }

    fn pretty_print_helper(&self, indent_level: usize) -> String {
        let content = match &self.node {
            AstNode::NumberNode(e) => format!("NumberNode({})", e),
            AstNode::BoolNode(e) => format!("BoolNode({})", e),
            AstNode::VarNode(e) => format!("VarNode({})", e),
            AstNode::LetNodeTopLevel(id, binding) => format!(
                "LetNodeTopLevel(id: {}, binding: {})",
                id,
                binding.pretty_print_helper(indent_level + 1)
            ),
            AstNode::LetNode(id, binding, body) => format!(
                "LetNode(id: {}, binding: {}, body: {})",
                id,
                binding.pretty_print_helper(indent_level + 1),
                body.pretty_print_helper(indent_level + 1)
            ),
            AstNode::IfNode(cond, consq, altern) => format!(
                "IfNode(cond: {}, consq: {}, altern: {})",
                cond.pretty_print_helper(indent_level + 1),
                consq.pretty_print_helper(indent_level + 1),
                altern.pretty_print_helper(indent_level + 1)
            ),
            AstNode::BinOpNode(op, e1, e2) => format!(
                "BinOpNode(op: {:?}, e1: {}, e2: {}",
                op,
                e1.pretty_print_helper(indent_level + 1),
                e2.pretty_print_helper(indent_level + 1)
            ),
            AstNode::FunCallNode(fun, args) => format!(
                "FunCallNode(fun: {}, args: {})",
                fun.pretty_print_helper(indent_level + 1),
                args.iter()
                    .map(|x| x.pretty_print_helper(indent_level + 1))
                    .collect::<Vec<String>>()
                    .join(",\n")
            ),
            AstNode::LambdaNode(params, body) => format!(
                "LambdaNode(params: {}, body: {})",
                params.join(", \n"),
                body.pretty_print_helper(indent_level + 1)
            ),
            AstNode::FunctionNode(name, params, body) => format!(
                "FunctionNode(name: {}, params: {}, body: {})",
                name,
                params.join(", "),
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
    Divide,
    Eq,
    Gt,
    Lt,
}
#[derive(PartialEq, Debug, Clone)]
pub enum Val<'a> {
    Num(i64),
    Bool(bool),
    Lam(&'a Vec<String>, &'a Ast, Env<'a>),
}

impl<'a> fmt::Display for Val<'a> {
    // This trait requires `fmt` with this exact signature.
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Val::Num(n) => write!(f, "Num({})", n),
            Val::Bool(v) => write!(f, "Bool({})", v),
            Val::Lam(_, _, _) => write!(f, "Lam"),
        }
    }
}
