use im::{HashMap, Vector};
use std::{fmt, ops::Range, usize};

pub type Env = HashMap<String, Val>;
pub type Program = Vec<Ast>;
#[derive(PartialEq, Debug, Clone, Hash)]
pub enum AstNode {
    /// (val)
    NumberNode(i64),
    /// (val)
    BoolNode(bool),
    /// (val)
    VarNode(Identifier),
    /// (id, expr)
    LetNodeTopLevel(Identifier, Box<Ast>),
    /// (id, expr, body)
    LetNode(Identifier, Box<Ast>, Box<Ast>),
    /// (conditions_and_bodies, alternate)
    IfNode(Vec<(Ast, Ast)>, Box<Ast>),
    /// (operator, operand1, operand2)
    BinOpNode(BinOp, Box<Ast>, Box<Ast>),
    /// (fun_value, arg_list)
    FunCallNode(Box<Ast>, Vec<Ast>),
    /// (param_list, body)
    LambdaNode(Vec<String>, Box<Ast>),
    /// (function_name, param_list, body)
    FunctionNode(String, Vec<Identifier>, Option<Type>, Box<Ast>),
    /// (data_name, data_Variants)
    DataDeclarationNode(String, Vec<(String, Vec<Identifier>)>),
    /// (discriminant, values)
    DataLiteralNode(String, Vec<Box<Ast>>),
    /// (expression_to_match, branches)
    MatchNode(Box<Ast>, Vec<(Pattern, Ast)>),
}

/// Represents an identifier. This includes identifiers used in let statements
/// as well as in function declarations. They may optionally have typ annotations
#[derive(PartialEq, Debug, Clone, Hash, Default)]
pub struct Identifier {
    pub id: String,
    pub type_decl: Option<Type>,
}
impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.type_decl {
            Some(t) => write!(f, "{}: {}", self.id.clone(), t),
            None => write!(f, "{}", self.id),
        }
    }
}
impl Identifier {
    pub fn new(id: String, type_decl: Option<Type>) -> Identifier {
        Identifier { id, type_decl }
    }
    pub fn new_without_type(id: String) -> Identifier {
        Identifier {
            id,
            type_decl: None,
        }
    }
    pub fn new_with_type(id: String, type_decl: Type) -> Identifier {
        Identifier {
            id,
            type_decl: Some(type_decl),
        }
    }
}

#[derive(PartialEq, Debug, Clone, Hash)]
pub struct SrcLoc {
    pub span: Range<usize>,
}
#[derive(PartialEq, Debug, Clone, Hash)]
pub struct Ast {
    pub node: AstNode,
    pub src_loc: SrcLoc,
}

impl Ast {
    pub fn pretty_print(&self) -> String {
        self.pretty_print_helper(0)
    }

    // TODO: clean up pretty printer
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
            AstNode::IfNode(conditions_and_bodies, altern) => format!(
                "IfNode(conditions_and_bodies: {}, altern: {})",
                conditions_and_bodies
                    .iter()
                    .map(|(cond, body)| format!(
                        "{}: {}",
                        cond.pretty_print_helper(indent_level + 1),
                        body.pretty_print_helper(indent_level + 2)
                    ))
                    .collect::<Vec<String>>()
                    .join(""),
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
            AstNode::FunctionNode(name, params, return_type, body) => format!(
                "FunctionNode(name: {}, params: {}, return_type: {:?}, body: {})",
                name,
                params
                    .iter()
                    .map(|param| format!("{}", param))
                    .collect::<Vec<String>>()
                    .join(", "),
                return_type,
                body.pretty_print_helper(indent_level + 1)
            ),
            AstNode::DataDeclarationNode(name, variants) => format!(
                "DataNode(name: {}, variants: {})",
                name,
                variants
                    .iter()
                    .map(|(name, fields)| format!(
                        "{}({}) ",
                        name,
                        fields
                            .iter()
                            .map(|x| format!("{}", x))
                            .collect::<Vec<String>>()
                            .join(", ")
                    ))
                    .collect::<Vec<String>>()
                    .join(" | "),
            ),
            AstNode::DataLiteralNode(discriminant, values) => format!(
                "DataLiteralNode(discriminant: {}, fields: {})",
                discriminant,
                values
                    .iter()
                    .map(|x| x.pretty_print_helper(indent_level + 1))
                    .collect::<Vec<String>>()
                    .join(",\n")
            ),
            AstNode::MatchNode(expression_to_match, branches) => format!(
                "MatchNode(expression_to_match: {}, branches: {})",
                expression_to_match.pretty_print_helper(indent_level + 1),
                branches
                    .iter()
                    .map(|(pattern, expr)| format!(
                        "{:?} => {}",
                        pattern,
                        expr.pretty_print_helper(indent_level + 1)
                    )
                    .to_string())
                    .collect::<Vec<String>>()
                    .join(",\n")
            ),
        };
        format!("\n{}{}", "\t".repeat(indent_level), content)
    }
}

#[derive(PartialEq, Debug, Clone, Hash)]
pub enum Pattern {
    NumLiteral(i64),
    BoolLiteral(bool),
    Data(String, Vec<Pattern>),
    Identifier(String),
}

#[derive(PartialEq, Debug, Clone, Copy, Hash)]
pub enum BinOp {
    Plus,
    Minus,
    Times,
    Divide,
    Modulo,
    Exp,
    Eq,
    Gt,
    Lt,
    GtEq,
    LtEq,
    LAnd,
    LOr,
    BitAnd,
    BitOr,
    BitXor,
}

/// Represents a Skiff type. This includes primitives like `Number`, but also more complex
/// types like `List<_>` and user-defined types.
#[derive(PartialEq, Debug, Clone, Hash, Default)]
pub struct Type {
    id: String,
    args: Vector<Type>,
}
impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.args.len() == 0 {
            write!(f, "{}", self.id)
        } else {
            write!(
                f,
                "{}<{}>",
                self.id,
                self.args
                    .iter()
                    .map(|arg| format!("{}", arg))
                    .collect::<Vec<String>>()
                    .join(", ")
            )
        }
    }
}
impl Type {
    pub fn new(id: String, args: Vector<Type>) -> Type {
        return Type { id, args };
    }
    pub fn new_unit(id: String) -> Type {
        return Type {
            id,
            args: Vector::new(),
        };
    }
    pub fn new_number() -> Type {
        return Type {
            id: "Number",
            args: Vector::new(),
        };
    }
    pub fn new_boolean() -> Type {
        return Type {
            id: "Boolean",
            args: Vector::new(),
        };
    }
    pub fn new_func(args: Vector<Type>, return_type: Type) -> Type {
        let mut combined_args_and_return = args.clone();
        combined_args_and_return.push_back(return_type);
        return Type {
            id: "Function".to_string(),
            args: combined_args_and_return,
        };
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Val {
    Num(i64),
    Bool(bool),
    Lam(Vec<String>, Ast, Env),
    // (discriminant, values)
    Data(String, Vec<Val>),
}

impl fmt::Display for Val {
    // This trait requires `fmt` with this exact signature.
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Val::Num(n) => write!(f, "Num({})", n),
            Val::Bool(v) => write!(f, "Bool({})", v),
            Val::Lam(_, _, _) => write!(f, "Lam"),
            Val::Data(discriminant, values) => write!(
                f,
                "{}({})",
                discriminant,
                values
                    .iter()
                    .map(|value| format!("{}", value))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
        }
    }
}
