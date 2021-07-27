use crate::ast::{Ast, AstNode, BinOp, Discriminant, Env, Pattern, Program, SrcLoc, Type, Val};
use crate::error_handling::add_position_info_to_filename;
use im::{vector, HashMap, Vector};
use std::convert::TryInto;
use std::fmt::Write;
use std::{borrow::Borrow, error};
use std::{fmt, ops::Range};

macro_rules! make_throw_interp_error {
    ($src_loc:expr, $env:expr, $stack:expr) => {
        macro_rules! throw_interp_error {
            ($msg:expr) => {
                return Err(InterpError(
                    $msg.to_string(),
                    $src_loc.span.clone(),
                    $env.clone(),
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
        filename: &std::path::PathBuf,
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

    pub fn print_stack(
        stack: &Vector<Self>,
        filename: &std::path::PathBuf,
        source: &str,
        printer: &mut impl Write,
    ) -> () {
        let _ = writeln!(printer, "Printing stack trace (most recent call last)");
        for (i, frame) in stack.iter().enumerate() {
            let _ = writeln!(printer, "{}", frame.pretty_print(i, filename, source));
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

#[derive(PartialEq, Debug, Clone, Copy, Hash)]
pub struct InterpretContext<'a> {
    pub env: &'a Env,
    pub func_table: &'a Env,
    pub stack: &'a Stack,
}
impl<'a> InterpretContext<'a> {
    pub fn new(env: &'a Env, func_table: &'a Env, stack: &'a Stack) -> Self {
        InterpretContext {
            env,
            func_table,
            stack,
        }
    }
    pub fn new_env(&self, env: &'a Env) -> Self {
        InterpretContext {
            env,
            func_table: self.func_table,
            stack: self.stack,
        }
    }
}

/// Interpret a Skiff program, possibly returning a runtime error
pub fn interpret(program: &Program) -> Result<Vec<Val>, InterpError> {
    // Find every data declaration in the program and add functions declarations
    // to the AST. Add a constructor function for each variant of each data declaration
    let data_funcs_ast = find_data_declarations(program)?;

    // Find all top level function declarations and put them into a map
    let funcs = find_functions(program)?;
    let data_funcs = find_functions(&data_funcs_ast)?;
    let funcs = funcs.into_iter().chain(data_funcs).collect();

    // Initialize state to keep track of top level definitions and values
    let mut env = HashMap::new();
    let mut vals = vec![];

    // Loop through each expression/declaration in the program and evaluate it.
    // The result is either value or a new binding in the environment.
    for expr in program {
        match interpret_top_level(expr, env.clone(), &funcs)? {
            ValOrEnv::V(val) => vals.push(val),
            ValOrEnv::E(new_env) => env = new_env,
        }
    }

    Ok(vals)
}

/// Find each top-level function declaration in a set of expressions and
/// put them in a map from name to AST body.
fn find_functions(program: &Program) -> Result<Env, InterpError> {
    let mut env: Env = HashMap::new();

    for expr in program {
        // Ignore the expression unless it's a function declaration
        match &expr.node {
            AstNode::FunctionNode(name, params, _, body) => {
                // Insert a lambda into the environment under the function's name
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

/// Find every data declaration in a program and add one constructor function to the progam
/// for each variant of each data declaration.
pub fn find_data_declarations(program: &Program) -> Result<Program, InterpError> {
    // Keep track of the functions we'll add to the AST
    let mut program_addendum = vec![];

    for expr in program {
        // Ignore the expression unless it's a data declaration
        match &expr {
            Ast {
                node: AstNode::DataDeclarationNode(name, variants),
                src_loc: SrcLoc { span },
                ..
            } => {
                // Add a function definition for each variant
                for (variant_name, variant_fields) in variants {
                    // Add a function body for the variant
                    let body = AstNode::DataLiteralNode(
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
                    );
                    // Add the function definition for the variant
                    let func = Ast::new(
                        AstNode::FunctionNode(
                            variant_name.clone(),
                            variant_fields.iter().cloned().collect(),
                            Some(Type::new(name.clone(), vector![])),
                            Box::new(Ast::new(body, SrcLoc { span: span.clone() })),
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

/// An enum representing either a Skiff value or runtime environment
enum ValOrEnv {
    V(Val),
    E(Env),
}

/// Interprets a top-level expression from a Skiff program. Result is either a value (for simple expression)
/// or a binding (for let expressions)
fn interpret_top_level(expr: &Ast, env: Env, func_table: &Env) -> Result<ValOrEnv, InterpError> {
    match &expr.node {
        // Add the let binding to the environment and return
        AstNode::LetNodeTopLevel(id, binding) => {
            let val = interpret_expr(
                binding.borrow(),
                InterpretContext::new(&env, func_table, &StackFrame::new_stack()),
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
        // Any other expression should be interpreted as a value
        _ => Ok(ValOrEnv::V(interpret_expr(
            expr,
            InterpretContext::new(&env, func_table, &StackFrame::new_stack()),
        )?)),
    }
}

/// Interprets a Skiff expression to produce either a value or an error
fn interpret_expr(expr: &Ast, context: InterpretContext) -> Result<Val, InterpError> {
    let InterpretContext {
        env,
        func_table,
        stack,
    } = context;

    make_throw_interp_error!(expr.src_loc, env, stack);

    match &expr.node {
        AstNode::NumberNode(n) => Ok(Val::Num(n.clone())),
        AstNode::BoolNode(v) => Ok(Val::Bool(v.clone())),
        // Variable nodes are looked up in the environment and then in the function table
        AstNode::VarNode(id) => match env.get(id) {
            Some(v) => Ok(v.clone()),
            None => match func_table.get(id) {
                Some(v) => Ok(v.clone()),
                None => throw_interp_error!(format!("Couldn't find var in environment: {}", id)),
            },
        },
        // Add the let binding to the environment and then interpret the body
        AstNode::LetNode(id, binding, body) => interpret_expr(
            body,
            context.new_env(&env.update(id.id.clone(), interpret_expr(binding, context)?)),
        ),
        AstNode::LetNodeTopLevel(_, _) => {
            throw_interp_error!("Found LetNodeTopLevel instead of LetNode in expression".to_string())
        }
        AstNode::BinOpNode(op, e1, e2) => {
            interpret_binop(*op, e1, e2, expr.src_loc.clone(), context)
        }
        AstNode::LambdaNode(params, body) => Ok(Val::Lam(
            params.iter().map(|id| id.id.clone()).collect(),
            *body.clone(),
            env.clone(),
        )),
        AstNode::FunCallNode(fun, args) => {
            // First, ensure that the value is a function
            let fun_value = interpret_expr(fun, context)?;
            match fun_value {
                Val::Lam(params, body, lam_env) => {
                    // Ensure the arg count of the function definition matches the arg count of the call
                    if params.len() != args.len() {
                        throw_interp_error!(format!(
                            "Function takes {} arguments but {} were provided",
                            params.len(),
                            args.len()
                        ));
                    }
                    // Create a new environment with the function's parameters bound to the arguments
                    let mut new_env: Env = HashMap::new();
                    for (param, arg) in params.iter().zip(args) {
                        new_env.insert(param.to_string(), interpret_expr(arg, context)?);
                    }

                    // Make the new stack and frame
                    let new_frame = StackFrame::new(expr.src_loc.clone(), new_env.clone());
                    let mut new_stack = stack.clone();
                    new_stack.push_back(new_frame);

                    // Make the new environment
                    let mut lam_env = lam_env.clone();
                    lam_env.extend(new_env);

                    // evaluate the body
                    interpret_expr(
                        &body,
                        InterpretContext::new(&lam_env, func_table, &new_stack),
                    )
                }
                _ => throw_interp_error!("Function call with non-function value".to_string()),
            }
        }
        AstNode::IfNode(conditions_and_bodies, alternate) => {
            // Loop through conditions in order and see if any match
            for (condition, body) in conditions_and_bodies {
                match interpret_expr(condition, context)? {
                    Val::Bool(true) => return interpret_expr(body, context),
                    Val::Bool(false) => continue,
                    _ => throw_interp_error!("Conditional expression with non-boolean condition"),
                }
            }
            // If no conditions match, then evaluate the alternate
            return interpret_expr(alternate, context);
        }
        AstNode::FunctionNode(_, _, _, _) => throw_interp_error!("Function node not at top level"),
        AstNode::DataDeclarationNode(_, _) => {
            throw_interp_error!("Found DataDeclarationNode instead of LetNode in expression")
        }
        AstNode::DataLiteralNode(discriminant, fields) => {
            // Create a data value with the proper discriminant
            let mut values = vec![];
            for expr in fields {
                values.push(interpret_expr(expr, context)?);
            }
            return Ok(Val::Data(discriminant.clone(), values));
        }
        AstNode::MatchNode(expression_to_match, branches) => {
            // Loop through each pattern and see if it matches. If it does, then evaluate
            // the body with the bindings from the pattern
            for (pattern, expr) in branches {
                let val = interpret_expr(expression_to_match, context)?;
                if let Some(match_env) = match_pattern_with_value(pattern, &val) {
                    return interpret_expr(expr, context.new_env(&env.clone().union(match_env)));
                }
            }
            throw_interp_error!("No branch of match expression matched value")
        }
    }
}

/// Attempts to match a pattern against a value. Returns None if the pattern doesn't match
/// or a set of bindings if the pattern does match
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
                // The pattern matches if the discriminants match and the pattern has the
                // right number of args
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
                $env.clone(),
                $stack.clone(),
            )),
            (e, Val::$type2(_)) => Err(InterpError(
                format!("Bad first op to {}: {}", stringify!($op), e).to_string(),
                $src_loc.span,
                $env.clone(),
                $stack.clone(),
            )),
            (e1, e2) => Err(InterpError(
                format!("Bad ops to {}: {}\n{}", stringify!($op), e1, e2).to_string(),
                $src_loc.span,
                $env.clone(),
                $stack.clone(),
            )),
        }
    };
}

/// Interprets a binary operation to produce a value or runtime error
fn interpret_binop(
    op: BinOp,
    e1: &Ast,
    e2: &Ast,
    src_loc: SrcLoc,
    context: InterpretContext,
) -> Result<Val, InterpError> {
    let InterpretContext { env, stack, .. } = context;

    make_throw_interp_error!(src_loc, env, stack);

    let v1 = interpret_expr(e1, context)?;
    let v2 = interpret_expr(e2, context)?;

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
