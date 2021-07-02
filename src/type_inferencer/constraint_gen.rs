use super::{
    ast::{Constraint, ConstraintSet, Term, TypeEnv},
    type_inference::InferenceError,
};
use crate::{
    ast::{Ast, AstNode, BinOp, Program, Symbol},
    interpreter::interpret::find_data_declarations,
};
use im::{hashset, vector, HashMap, HashSet};

fn new_constraint(t1: Term, t2: Term) -> Constraint {
    return (t1, t2);
}
fn singleton_constraint(t1: Term, t2: Term) -> ConstraintSet {
    HashSet::unit((t1, t2))
}
fn empty_constraint_set() -> ConstraintSet {
    HashSet::new()
}

pub fn find_in_env(id: &str, env: TypeEnv) -> Result<Symbol, InferenceError> {
    match env.get(id) {
        Some(v) => Ok(v.clone()),
        None => Err(InferenceError::UnboundIdentifier(id.to_string())),
    }
}

pub fn generate_constraints(program: &Program) -> Result<ConstraintSet, InferenceError> {
    let data_funcs_ast = match find_data_declarations(&program) {
        Ok(v) => Ok(v),
        Err(e) => Err(InferenceError::DataDeclarationError(e)),
    }?;

    // Find functions and functions to make ADT literals
    let (user_funcs_constraints, user_funcs) = find_functions(&program)?;
    let (data_funcs_constraints, data_funcs) = find_functions(&data_funcs_ast)?;
    let func_table = user_funcs.union(data_funcs);

    let mut env: TypeEnv = HashMap::new();
    let mut constraint_set = empty_constraint_set();
    constraint_set = constraint_set.union(user_funcs_constraints);
    constraint_set = constraint_set.union(data_funcs_constraints);

    for expr in program {
        let (new_constraint_set, new_env) =
            generate_constraints_top_level(&expr, env, &func_table)?;

        env = new_env;
        constraint_set = constraint_set.union(new_constraint_set);
    }

    Ok(constraint_set)
}

fn find_functions(program: &Program) -> Result<(ConstraintSet, TypeEnv), InferenceError> {
    let mut env: TypeEnv = HashMap::new();
    let mut constraint_set = empty_constraint_set();
    for expr in program {
        match &expr.node {
            AstNode::FunctionNode(name, params, return_type, _body) => {
                // ensure the function has full type annotations
                let mut param_types = vector![];
                for param in params {
                    match param.type_decl.clone() {
                        Some(t) => param_types.push_back(Term::from_type(&t)),
                        None => {
                            return Err(InferenceError::MissingAnnotation(
                                expr.src_loc.span.clone(),
                            ))
                        }
                    }
                }
                let return_type = match return_type {
                    Some(t) => Ok(t),
                    None => Err(InferenceError::MissingAnnotation(expr.src_loc.span.clone())),
                }?;

                // TODO: possibly more constraints here
                constraint_set.insert((
                    Term::Var(expr.label),
                    Term::function(param_types, Term::from_type(return_type)),
                ));
                env.insert(name.clone(), expr.label);
                // env.insert(
                //     name.clone(),
                //     Val::Lam(
                //         params
                //             .iter()
                //             .map(|param| param.id.clone())
                //             .collect::<Vec<String>>(),
                //         *body.clone(),
                //         HashMap::new(),
                //     ),
                // );
            }
            _ => (),
        };
    }

    return Ok((constraint_set, env));
}

fn generate_constraints_top_level(
    expr: &Ast,
    env: TypeEnv,
    func_table: &TypeEnv,
) -> Result<(ConstraintSet, TypeEnv), InferenceError> {
    match &expr.node {
        AstNode::LetNodeTopLevel(id, binding) => {
            let body_constraints = generate_constraint_expr(binding, env.clone(), func_table)?;
            let let_constraint =
                singleton_constraint(Term::Var(id.label), Term::Var(binding.label));

            let mut new_env = env.clone();
            new_env.insert(id.id.clone(), binding.label);

            Ok((body_constraints.union(let_constraint), new_env))
        }
        AstNode::LetNode(_, _, _) => Err(InferenceError::TopLevelError(expr.src_loc.clone())),
        AstNode::FunctionNode(_, _, _, _) => Ok((empty_constraint_set(), env)),
        AstNode::DataDeclarationNode(_, _) => Ok((empty_constraint_set(), env)),
        _ => Ok((
            generate_constraint_expr(expr, env.clone(), func_table)?,
            env,
        )),
    }
}

pub fn generate_constraint_expr(
    expr: &Ast,
    env: TypeEnv,
    func_table: &TypeEnv,
) -> Result<ConstraintSet, InferenceError> {
    match &expr.node {
        AstNode::NumberNode(_val) => {
            Ok(singleton_constraint(Term::Var(expr.label), Term::number()))
        }
        AstNode::BoolNode(_val) => Ok(singleton_constraint(Term::Var(expr.label), Term::boolean())),
        AstNode::VarNode(id) => Ok(singleton_constraint(
            Term::Var(expr.label),
            Term::Var(find_in_env(id, env)?),
        )),
        AstNode::LetNodeTopLevel(_id, expr) => Err(InferenceError::TopLevelExpressionOutOfPlace(
            expr.src_loc.clone(),
        )),
        AstNode::LetNode(id, expr, body) => {
            let mut new_env = env.clone();
            new_env.insert(id.id.clone(), expr.label);
            let expr_constraints = generate_constraint_expr(&expr, env, func_table)?;
            let body_constraints = generate_constraint_expr(&body, new_env, func_table)?;
            Ok(expr_constraints.union(body_constraints))
        }
        AstNode::IfNode(conditions_and_bodies, alternate) => {
            let mut last_term = None;
            let mut constraints = empty_constraint_set();
            for (condition, body) in conditions_and_bodies {
                constraints = constraints.union(generate_constraint_expr(
                    &condition,
                    env.clone(),
                    func_table,
                )?);
                constraints =
                    constraints.union(generate_constraint_expr(&body, env.clone(), func_table)?);
                constraints = constraints.union(singleton_constraint(
                    Term::Var(condition.label),
                    Term::boolean(),
                ));
                if let Some(v) = last_term {
                    constraints = constraints.union(singleton_constraint(v, Term::Var(body.label)));
                    last_term = Some(Term::Var(body.label));
                }
            }

            constraints = constraints.union(generate_constraint_expr(&alternate, env, func_table)?);

            if let Some(v) = last_term {
                constraints =
                    constraints.union(singleton_constraint(v, Term::Var(alternate.label)));
            }

            // The overall expression must have same type as all branches
            constraints.insert((Term::Var(expr.label), Term::Var(alternate.label)));

            Ok(constraints)
        }
        AstNode::BinOpNode(op, e1, e2) => {
            constraint_gen_binop(op.clone(), expr.label, &e1, &e2, env, func_table)
        }
        AstNode::FunCallNode(fun_value, arg_list) => {
            // 1. expressions type is a value
            // 2. functions type is a function
            let arg_terms = arg_list.iter().map(|arg| Term::Var(arg.label)).collect();
            let new_constraint = singleton_constraint(
                Term::Var(fun_value.label),
                Term::function(arg_terms, Term::Var(expr.label)),
            );

            let mut arg_constraints = vec![];
            for arg in arg_list {
                arg_constraints.push(generate_constraint_expr(&arg, env.clone(), func_table)?);
            }
            let arg_constraints = HashSet::unions(arg_constraints);

            let fun_constraints = generate_constraint_expr(&fun_value, env, func_table)?;

            Ok(fun_constraints.union(new_constraint).union(arg_constraints))
        }
        AstNode::LambdaNode(param_list, body) => {
            let mut lam_env = env.clone();
            for param in param_list {
                lam_env.insert(param.id.clone(), param.label);
            }
            let body_constraints = generate_constraint_expr(&body, lam_env, func_table)?;

            let param_labels = param_list
                .iter()
                .map(|param| Term::Var(param.label))
                .collect();
            let param_constraints = singleton_constraint(
                Term::Var(expr.label),
                Term::function(param_labels, Term::Var(body.label)),
            );

            Ok(body_constraints.union(param_constraints))
        }
        AstNode::FunctionNode(_function_name, _param_list, _return_type, _body) => Err(
            InferenceError::TopLevelExpressionOutOfPlace(expr.src_loc.clone()),
        ),
        AstNode::DataDeclarationNode(_data_name, _data_variants) => Err(
            InferenceError::TopLevelExpressionOutOfPlace(expr.src_loc.clone()),
        ),
        AstNode::DataLiteralNode(discriminant, _values) => Ok(singleton_constraint(
            Term::Var(expr.label),
            Term::Constructor(discriminant.clone(), vector![]),
        )),
        AstNode::MatchNode(_expression_to_match, branches) => {
            let mut last_term: Option<Term> = None;
            let mut constraints = empty_constraint_set();
            for (_pattern, body) in branches {
                constraints =
                    constraints.union(generate_constraint_expr(&body, env.clone(), func_table)?);
                if let Some(v) = last_term {
                    constraints = constraints.union(singleton_constraint(v, Term::Var(body.label)));

                    last_term = Some(Term::Var(body.label));
                }
            }

            Ok(constraints)
        }
    }
}

pub fn constraint_gen_binop_helper(
    label: Symbol,
    left_label: Symbol,
    right_label: Symbol,
    left_type: Term,
    right_type: Term,
    output_type: Term,
) -> ConstraintSet {
    hashset![
        new_constraint(Term::Var(left_label), left_type),
        new_constraint(Term::Var(right_label), right_type),
        new_constraint(Term::Var(label), output_type),
    ]
}

fn constraint_gen_binop(
    op: BinOp,
    label: Symbol,
    e1: &Ast,
    e2: &Ast,
    env: TypeEnv,
    func_table: &TypeEnv,
) -> Result<ConstraintSet, InferenceError> {
    let c1 = generate_constraint_expr(e1, env.clone(), func_table)?;
    let c2 = generate_constraint_expr(e2, env, func_table)?;

    let c3 = match op {
        BinOp::Plus => constraint_gen_binop_helper(
            label,
            e1.label,
            e2.label,
            Term::number(),
            Term::number(),
            Term::number(),
        ),
        BinOp::Minus => constraint_gen_binop_helper(
            label,
            e1.label,
            e2.label,
            Term::number(),
            Term::number(),
            Term::number(),
        ),
        BinOp::Times => constraint_gen_binop_helper(
            label,
            e1.label,
            e2.label,
            Term::number(),
            Term::number(),
            Term::number(),
        ),
        BinOp::Divide => constraint_gen_binop_helper(
            label,
            e1.label,
            e2.label,
            Term::number(),
            Term::number(),
            Term::number(),
        ),
        BinOp::Modulo => constraint_gen_binop_helper(
            label,
            e1.label,
            e2.label,
            Term::number(),
            Term::number(),
            Term::number(),
        ),
        BinOp::Exp => constraint_gen_binop_helper(
            label,
            e1.label,
            e2.label,
            Term::number(),
            Term::number(),
            Term::number(),
        ),
        BinOp::Eq => constraint_gen_binop_helper(
            label,
            e1.label,
            e2.label,
            Term::number(),
            Term::number(),
            Term::number(),
        ),
        BinOp::Gt => constraint_gen_binop_helper(
            label,
            e1.label,
            e2.label,
            Term::number(),
            Term::number(),
            Term::boolean(),
        ),
        BinOp::Lt => constraint_gen_binop_helper(
            label,
            e1.label,
            e2.label,
            Term::number(),
            Term::number(),
            Term::boolean(),
        ),
        BinOp::GtEq => constraint_gen_binop_helper(
            label,
            e1.label,
            e2.label,
            Term::number(),
            Term::number(),
            Term::boolean(),
        ),
        BinOp::LtEq => constraint_gen_binop_helper(
            label,
            e1.label,
            e2.label,
            Term::number(),
            Term::number(),
            Term::boolean(),
        ),
        BinOp::LAnd => constraint_gen_binop_helper(
            label,
            e1.label,
            e2.label,
            Term::boolean(),
            Term::boolean(),
            Term::boolean(),
        ),
        BinOp::LOr => constraint_gen_binop_helper(
            label,
            e1.label,
            e2.label,
            Term::boolean(),
            Term::boolean(),
            Term::boolean(),
        ),
        BinOp::BitAnd => constraint_gen_binop_helper(
            label,
            e1.label,
            e2.label,
            Term::number(),
            Term::number(),
            Term::number(),
        ),
        BinOp::BitOr => constraint_gen_binop_helper(
            label,
            e1.label,
            e2.label,
            Term::number(),
            Term::number(),
            Term::number(),
        ),
        BinOp::BitXor => constraint_gen_binop_helper(
            label,
            e1.label,
            e2.label,
            Term::number(),
            Term::number(),
            Term::number(),
        ),
    };

    return Ok(c1.union(c2).union(c3));
}
