use crate::ast::{Ast, AstNode, BinOp, Discriminant, Env, Pattern, Program, SrcLoc, Type, Val};
use crate::error_handling::add_position_info_to_filename;
use im::{vector, HashMap, Vector};
use std::convert::TryInto;
use std::{borrow::Borrow, error};
use std::{fmt, ops::Range};

macro_rules! make_throw_interp_error {
    ($src_loc:expr, $env:expr, $stack:expr) => {
        macro_rules! throw_interp_error {
            ($msg:expr) => {
                return Err(InterpError(
                    $msg.to_string(),
                    $src_loc.span.clone(),
                    $env,
                    $stack.clone(),
                ));
            };
        }
    };
}

#[derive(PartialEq, Debug, Clone, Hash)]
pub struct StackFrame {
    src_loc: SrcLoc,
    arg_environment: Env,
}
impl StackFrame {
    fn new(src_loc: SrcLoc, arg_environment: Env) -> StackFrame {
        StackFrame {
            src_loc,
            arg_environment,
        }
    }
    fn new_stack() -> Stack {
        Vector::unit(StackFrame {
            src_loc: SrcLoc { span: 0..0 },
            arg_environment: HashMap::new(),
        })
    }
    // TODO: incorporate arg_environment in pretty printing
    pub fn pretty_print(
        &self,
        stack_index: usize,
        filename: std::path::PathBuf,
        source: &str,
    ) -> String {
        match self {
            StackFrame {
                src_loc: SrcLoc { span },
                arg_environment: _arg_environment,
            } => format!(
                "#{}: {}\n\t{}",
                stack_index,
                add_position_info_to_filename(source, span.start, filename),
                source[span.start..span.end].to_string(),
            )
            .to_string(),
        }
    }
}
type Stack = Vector<StackFrame>;

#[derive(PartialEq, Debug, Clone, Hash)]
pub struct InterpError(pub String, pub Range<usize>, pub Env, pub Stack);
impl fmt::Display for InterpError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}
impl error::Error for InterpError {}

pub fn interpret(program: &Program) -> Result<Vec<Val>, InterpError> {
    let data_funcs_ast = find_data_declarations(program)?;

    // Find functions and functions to make ADT literals
    let funcs = find_functions(program)?;
    let data_funcs = find_functions(&data_funcs_ast)?;
    let funcs = funcs.into_iter().chain(data_funcs).collect();

    let mut env = HashMap::new();
    let mut vals = vec![];

    for expr in program {
        match interpret_top_level(expr, env.clone(), &funcs)? {
            ValOrEnv::V(val) => vals.push(val),
            ValOrEnv::E(new_env) => env = new_env,
        }
    }

    Ok(vals)
}

fn find_functions(program: &Program) -> Result<Env, InterpError> {
    let mut env: Env = HashMap::new();

    for expr in program {
        match &expr.node {
            AstNode::FunctionNode(name, params, _, body) => {
                env.insert(
                    name.clone(),
                    Val::Lam(
                        params
                            .iter()
                            .map(|param| param.id.clone())
                            .collect::<Vec<String>>(),
                        *body.clone(),
                        HashMap::new(),
                    ),
                );
            }
            _ => (),
        };
    }

    return Ok(env);
}

pub fn find_data_declarations(program: &Program) -> Result<Program, InterpError> {
    let mut program_addendum = vec![];

    for expr in program {
        match &expr {
            Ast {
                node: AstNode::DataDeclarationNode(name, variants),
                src_loc: SrcLoc { span },
                ..
            } => {
                for (variant_name, variant_fields) in variants {
                    let func = Ast::new(
                        AstNode::FunctionNode(
                            variant_name.clone(),
                            variant_fields.iter().cloned().collect(),
                            Some(Type::new(name.clone(), vector![])),
                            Box::new(Ast::new(
                                AstNode::DataLiteralNode(
                                    Discriminant::new(name, &variant_name),
                                    variant_fields
                                        .iter()
                                        .map(|id| {
                                            Box::new(Ast::new(
                                                AstNode::VarNode(id.id.clone()),
                                                SrcLoc { span: span.clone() },
                                            ))
                                        })
                                        .collect(),
                                ),
                                SrcLoc { span: span.clone() },
                            )),
                        ),
                        SrcLoc { span: span.clone() },
                    );
                    program_addendum.push(func);
                }
            }
            _ => (),
        };
    }

    return Ok(program_addendum);
}

enum ValOrEnv {
    V(Val),
    E(Env),
}

fn interpret_top_level(expr: &Ast, env: Env, func_table: &Env) -> Result<ValOrEnv, InterpError> {
    match &expr.node {
        AstNode::LetNodeTopLevel(id, binding) => {
            let val = interpret_expr(
                binding.borrow(),
                env.clone(),
                func_table,
                &StackFrame::new_stack(),
            )?;
            Ok(ValOrEnv::E(env.update(id.id.clone(), val)))
        }
        AstNode::LetNode(_, _, _) => Err(InterpError(
            "Found LetNode instead of LetNodeToplevel on top level".to_string(),
            expr.src_loc.span.clone(),
            env,
            StackFrame::new_stack(),
        )),
        AstNode::FunctionNode(_, _, _, _) => Ok(ValOrEnv::E(env)),
        AstNode::DataDeclarationNode(_, _) => Ok(ValOrEnv::E(env)),
        _ => Ok(ValOrEnv::V(interpret_expr(
            expr,
            env,
            func_table,
            &StackFrame::new_stack(),
        )?)),
    }
}

fn interpret_expr(
    expr: &Ast,
    env: Env,
    func_table: &Env,
    stack: &Stack,
) -> Result<Val, InterpError> {
    make_throw_interp_error!(expr.src_loc, env, stack);
    match &expr.node {
        AstNode::NumberNode(n) => Ok(Val::Num(n.clone())),
        AstNode::BoolNode(v) => Ok(Val::Bool(v.clone())),
        AstNode::VarNode(id) => match env.get(id) {
            Some(v) => Ok(v.clone()),
            None => match func_table.get(id) {
                Some(v) => Ok(v.clone()),
                None => throw_interp_error!(format!("Couldn't find var in environment: {}", id)),
            },
        },
        AstNode::LetNode(id, binding, body) => interpret_expr(
            body,
            env.update(
                id.id.clone(),
                interpret_expr(binding, env.clone(), func_table, stack)?,
            ),
            func_table,
            stack,
        ),
        AstNode::LetNodeTopLevel(_, _) => {
            throw_interp_error!("Found LetNodeTopLevel instead of LetNode in expression".to_string())
        }
        AstNode::BinOpNode(op, e1, e2) => {
            interpret_binop(*op, e1, e2, expr.src_loc.clone(), env, func_table, stack)
        }
        AstNode::LambdaNode(params, body) => Ok(Val::Lam(
            params.iter().map(|id| id.id.clone()).collect(),
            *body.clone(),
            env.clone(),
        )),
        AstNode::FunCallNode(fun, args) => {
            let fun_value = interpret_expr(fun, env.clone(), func_table, stack)?;
            match fun_value {
                Val::Lam(params, body, lam_env) => {
                    if params.len() != args.len() {
                        throw_interp_error!(format!(
                            "Function takes {} arguments but {} were provided",
                            params.len(),
                            args.len()
                        ));
                    }

                    let mut new_env: Env = HashMap::new();
                    for (param, arg) in params.iter().zip(args) {
                        new_env.insert(
                            param.to_string(),
                            interpret_expr(arg, env.clone(), func_table, stack)?,
                        );
                    }
                    // Make the new stack and frame
                    let new_frame = StackFrame::new(expr.src_loc.clone(), new_env.clone());
                    let mut new_stack = stack.clone();
                    new_stack.push_back(new_frame);
                    // Make the new environment
                    let mut lam_env = lam_env.clone();
                    lam_env.extend(new_env);
                    // evaluate the body
                    interpret_expr(&body, lam_env, func_table, &new_stack)
                }
                _ => throw_interp_error!("Function call with non-function value".to_string()),
            }
        }
        AstNode::IfNode(conditions_and_bodies, alternate) => {
            for (condition, body) in conditions_and_bodies {
                match interpret_expr(condition, env.clone(), func_table, stack)? {
                    Val::Bool(true) => return interpret_expr(body, env.clone(), func_table, stack),
                    Val::Bool(false) => continue,
                    _ => throw_interp_error!("Conditional expression with non-boolean condition"),
                }
            }
            return interpret_expr(alternate, env.clone(), func_table, stack);
        }
        AstNode::FunctionNode(_, _, _, _) => throw_interp_error!("Function node not at top level"),
        AstNode::DataDeclarationNode(_, _) => {
            throw_interp_error!("Found DataDeclarationNode instead of LetNode in expression")
        }
        AstNode::DataLiteralNode(discriminant, fields) => {
            let mut values = vec![];
            for expr in fields {
                values.push(interpret_expr(expr, env.clone(), func_table, stack)?);
            }
            return Ok(Val::Data(discriminant.clone(), values));
        }
        AstNode::MatchNode(expression_to_match, branches) => {
            for (pattern, expr) in branches {
                let val = interpret_expr(expression_to_match, env.clone(), func_table, stack)?;
                if let Some(match_env) = match_pattern_with_value(pattern, &val) {
                    return interpret_expr(expr, env.union(match_env), func_table, stack);
                }
            }
            throw_interp_error!("No branch of match expression matched value")
        }
    }
}

fn match_pattern_with_value(pattern: &Pattern, value: &Val) -> Option<Env> {
    match pattern {
        Pattern::BoolLiteral(b) => {
            if *value == Val::Bool(*b) {
                Some(HashMap::new())
            } else {
                None
            }
        }
        Pattern::NumLiteral(n) => {
            if *value == Val::Num(*n) {
                Some(HashMap::new())
            } else {
                None
            }
        }
        Pattern::Identifier(s) => {
            if s == "_" {
                Some(HashMap::new())
            } else {
                Some(HashMap::unit(s.clone(), value.clone()))
            }
        }
        Pattern::Data(pattern_discriminant, patterns) => match value {
            Val::Data(value_discriminant, values) => {
                if pattern_discriminant == value_discriminant.get_variant() {
                    if patterns.len() == values.len() {
                        let mut env = HashMap::new();
                        for (pattern, value) in patterns.into_iter().zip(values) {
                            env = env.union(match_pattern_with_value(pattern, value)?);
                        }
                        Some(env)
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            _ => None,
        },
    }
}

macro_rules! interpret_binop {
    ($value1:ident, $value2:ident, $src_loc: expr, $op:tt, $type1:ident, $type2:ident, $output_type:ident, $env:ident, $stack:ident) => {
        match ($value1, $value2) {
            (Val::$type1(xv), Val::$type2(yv)) => Ok(Val::$output_type(xv $op yv)),
            (Val::$type1(_), e) => Err(InterpError(
                format!("Bad second op to {}: {}", stringify!($op), e).to_string(),
                $src_loc.span,
                $env,
                $stack.clone(),
            )),
            (e, Val::$type2(_)) => Err(InterpError(
                format!("Bad first op to {}: {}", stringify!($op), e).to_string(),
                $src_loc.span,
                $env,
                $stack.clone(),
            )),
            (e1, e2) => Err(InterpError(
                format!("Bad ops to {}: {}\n{}", stringify!($op), e1, e2).to_string(),
                $src_loc.span,
                $env,
                $stack.clone(),
            )),
        }
    };
}

fn interpret_binop(
    op: BinOp,
    e1: &Ast,
    e2: &Ast,
    src_loc: SrcLoc,
    env: Env,
    func_table: &Env,
    stack: &Stack,
) -> Result<Val, InterpError> {
    make_throw_interp_error!(src_loc, env, stack);

    let v1 = interpret_expr(e1, env.clone(), func_table, stack)?;
    let v2 = interpret_expr(e2, env.clone(), func_table, stack)?;

    match op {
        BinOp::Plus => interpret_binop!(v1, v2, src_loc, +, Num, Num, Num, env, stack),
        BinOp::Minus => interpret_binop!(v1, v2, src_loc, -, Num, Num, Num, env, stack),
        BinOp::Times => interpret_binop!(v1, v2, src_loc, *, Num, Num, Num, env, stack),
        BinOp::Divide => interpret_binop!(v1, v2, src_loc, /, Num, Num, Num, env, stack),
        BinOp::Modulo => interpret_binop!(v1, v2, src_loc, %, Num, Num, Num, env, stack),
        BinOp::Exp => match (v1, v2) {
            (Val::Num(xv), Val::Num(yv)) => Ok(Val::Num(xv.pow(yv.try_into().unwrap()))),
            (Val::Num(_), e) => throw_interp_error!(format!("Bad second op to {}: {}", "**", e)),
            (e, Val::Num(_)) => throw_interp_error!(format!("Bad first op to {}: {}", "**", e)),
            (e1, e2) => throw_interp_error!(format!("Bad ops to {}: {}\n{}", "**", e1, e2)),
        },
        BinOp::Eq => Ok(Val::Bool(v1 == v2)),
        BinOp::Gt => interpret_binop!(v1, v2, src_loc, >, Num, Num, Bool, env, stack),
        BinOp::Lt => interpret_binop!(v1, v2, src_loc, <, Num, Num, Bool, env, stack),
        BinOp::GtEq => interpret_binop!(v1, v2, src_loc, >=, Num, Num, Bool, env, stack),
        BinOp::LtEq => interpret_binop!(v1, v2, src_loc, <=, Num, Num, Bool, env, stack),
        BinOp::LAnd => interpret_binop!(v1, v2, src_loc, &&, Bool, Bool, Bool, env, stack),
        BinOp::LOr => interpret_binop!(v1, v2, src_loc, ||, Bool, Bool, Bool, env, stack),
        BinOp::BitAnd => interpret_binop!(v1, v2, src_loc, &, Num, Num, Num, env, stack),
        BinOp::BitOr => interpret_binop!(v1, v2, src_loc, |, Num, Num, Num, env, stack),
        BinOp::BitXor => interpret_binop!(v1, v2, src_loc, ^, Num, Num, Num, env, stack),
    }
}
