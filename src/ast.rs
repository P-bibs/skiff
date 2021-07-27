use colored::Colorize;
use im::{HashMap, Vector};
use std::sync::Mutex;
use std::{fmt, ops::Range, usize};

pub type Symbol = usize;
lazy_static! {
    static ref GENSYM_COUNTER: Mutex<usize> = Mutex::new(0);
}
pub fn gensym() -> Symbol {
    let mut gs = GENSYM_COUNTER.lock().unwrap();
    *gs = *gs + 1;
    return *gs;
}

pub type Env = HashMap<String, Val>;
pub type Program = Vec<Ast>;
#[derive(PartialEq, Debug, Clone, Hash)]
pub enum AstNode {
    /// (val)
    NumberNode(i64),
    /// (val)
    BoolNode(bool),
    /// (val)
    VarNode(String),
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
    LambdaNode(Vec<Identifier>, Box<Ast>),
    /// (function_name, param_list, body)
    FunctionNode(String, Vec<Identifier>, Option<Type>, Box<Ast>),
    /// (data_name, data_Variants)
    DataDeclarationNode(String, Vec<(String, Vec<Identifier>)>),
    /// (discriminant, values)
    DataLiteralNode(Discriminant, Vec<Box<Ast>>),
    /// (expression_to_match, branches)
    MatchNode(Box<Ast>, Vec<(Pattern, Ast)>),
}

/// Represents an identifier. This includes identifiers used in let statements
/// as well as in function declarations. They may optionally have typ annotations
#[derive(PartialEq, Debug, Clone, Hash, Default)]
pub struct Identifier {
    pub id: String,
    pub type_decl: Option<Type>,
    pub label: Symbol,
}
impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.type_decl {
            Some(t) => write!(
                f,
                "{}: {}: {}",
                format!("{}", self.label).blue(),
                self.id.clone(),
                t
            ),
            None => write!(f, "{}: {}", format!("{}", self.label).blue(), self.id),
        }
    }
}
impl Identifier {
    pub fn new(id: String, type_decl: Option<Type>) -> Identifier {
        Identifier {
            id,
            type_decl,
            label: gensym(),
        }
    }
    pub fn new_without_type(id: String) -> Identifier {
        Identifier {
            id,
            type_decl: None,
            label: gensym(),
        }
    }
    pub fn new_with_type(id: String, type_decl: Type) -> Identifier {
        Identifier {
            id,
            type_decl: Some(type_decl),
            label: gensym(),
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
    pub label: Symbol,
}

impl Ast {
    pub fn new(node: AstNode, src_loc: SrcLoc) -> Ast {
        return Ast {
            node,
            src_loc,
            label: gensym(),
        };
    }

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
                "BinOpNode(op: {:?}, e1: {}, e2: {})",
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
                params
                    .iter()
                    .map(|param| format!("{}", param))
                    .collect::<Vec<String>>()
                    .join(", \n"),
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
        format!(
            "\n{:4}:{}{}",
            format!("{}", self.label).blue(),
            "\t".repeat(indent_level),
            content
        )
    }

    pub fn into_vec(&self) -> Vec<&Ast> {
        let mut out = vec![self];
        match &self.node {
            AstNode::NumberNode(_) | AstNode::BoolNode(_) | AstNode::VarNode(_) => (),
            // Add the let binding to the environment and then interpret the body
            AstNode::LetNode(_, binding, body) => {
                out.extend(binding.into_vec());
                out.extend(body.into_vec());
            }
            AstNode::LetNodeTopLevel(_, binding) => out.extend(binding.into_vec()),
            AstNode::BinOpNode(_, e1, e2) => {
                out.extend(e1.into_vec());
                out.extend(e2.into_vec());
            }
            AstNode::LambdaNode(_, body) => out.extend(body.into_vec()),
            AstNode::FunCallNode(fun, args) => {
                out.extend(fun.into_vec());
                for arg in args {
                    out.extend(arg.into_vec());
                }
            }
            AstNode::IfNode(conditions_and_bodies, alternate) => {
                for (condition, body) in conditions_and_bodies {
                    out.extend(condition.into_vec());
                    out.extend(body.into_vec());
                }
                out.extend(alternate.into_vec());
            }
            AstNode::FunctionNode(_, _, _, body) => {
                out.extend(body.into_vec());
            }
            AstNode::DataDeclarationNode(_, _) => (),
            AstNode::DataLiteralNode(_, fields) => {
                for field in fields {
                    out.extend(field.into_vec());
                }
            }
            AstNode::MatchNode(expression_to_match, branches) => {
                out.extend(expression_to_match.into_vec());
                for (_, expr) in branches {
                    out.extend(expr.into_vec());
                }
            }
        };
        return out;
    }
}

#[derive(Eq, PartialEq, Debug, Clone, Hash, Default)]
pub struct Discriminant {
    source_type: String,
    variant: String,
}
impl Discriminant {
    pub fn new(source_type: &str, variant: &str) -> Self {
        Discriminant {
            source_type: source_type.to_string(),
            variant: variant.to_string(),
        }
    }
    pub fn get_type(&self) -> &str {
        &self.source_type
    }
    pub fn get_variant(&self) -> &str {
        &self.variant
    }
}
impl fmt::Display for Discriminant {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.variant)
    }
}

#[derive(PartialEq, Debug, Clone, Hash)]
pub enum Pattern {
    NumLiteral(i64),
    BoolLiteral(bool),
    Data(String, Vec<Pattern>),
    Identifier(String),
}

impl Pattern {
    /// Returns `true` if the pattern is [`NumLiteral`].
    pub fn is_num_literal(&self) -> bool {
        matches!(self, Self::NumLiteral(..))
    }

    /// Returns `true` if the pattern is [`BoolLiteral`].
    pub fn is_bool_literal(&self) -> bool {
        matches!(self, Self::BoolLiteral(..))
    }

    /// Returns `true` if the pattern is [`Data`].
    pub fn is_data(&self) -> bool {
        matches!(self, Self::Data(..))
    }

    /// Returns `true` if the pattern is [`Identifier`].
    pub fn is_identifier(&self) -> bool {
        matches!(self, Self::Identifier(..))
    }
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
#[derive(Eq, PartialEq, Debug, Clone, Hash, Default)]
pub struct Type {
    pub id: String,
    pub args: Vector<Type>,
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
            id: "Number".to_string(),
            args: Vector::new(),
        };
    }
    pub fn new_boolean() -> Type {
        return Type {
            id: "Boolean".to_string(),
            args: Vector::new(),
        };
    }
    pub fn new_any() -> Type {
        return Type {
            id: "Any".to_string(),
            args: Vector::new(),
        };
    }
    pub fn none_to_any(type_decl: Option<Type>) -> Option<Type> {
        match type_decl {
            None => Some(Self::new_any()),
            _ => type_decl,
        }
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

#[derive(PartialEq, Debug, Clone, Hash)]
pub enum Val {
    Num(i64),
    Bool(bool),
    Lam(Vec<String>, Ast, Env),
    // (discriminant, values)
    Data(Discriminant, Vec<Val>),
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
