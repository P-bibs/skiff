use std::fmt;

#[derive(PartialEq, Debug, Clone)]
pub enum Ast {
    // (defns, expr)
    ProgramNode(Vec<Ast>),
    // (val)
    NumberNode(i64),
    // (val)
    VarNode(String),
    // (id, expr)
    LetNode(String, Box<Ast>),
    // (operator, operand1, operand2)
    BinOpNode(BinOp, Box<Ast>, Box<Ast>),
    // (fun_value, arg_list)
    FunCallNode(Box<Ast>, Vec<Ast>),
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum BinOp {
    Plus,
    Times,
}
#[derive(PartialEq, Debug, Clone, Copy)]
pub enum Val {
    Num(i64),
    Unit,
}
impl fmt::Display for Val {
    // This trait requires `fmt` with this exact signature.
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Val::Num(n) => write!(f, "Num({})", n),
            Val::Unit => write!(f, "Unit()"),
        }
    }
}
