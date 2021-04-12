use std::fmt;

#[derive(PartialEq, Debug, Clone)]
pub enum Ast {
    NumberNode(i64),
    BinOpNode(BinOp, Box<Ast>, Box<Ast>),
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum BinOp {
    Plus,
    Times,
}
#[derive(PartialEq, Debug, Clone, Copy)]
pub enum Val {
    Num(i64),
}
impl fmt::Display for Val {
    // This trait requires `fmt` with this exact signature.
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Val::Num(n) => write!(f, "Num({})", n),
        }
    }
}
