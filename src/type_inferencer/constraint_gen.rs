use super::{
    ast::{ConstraintSet, Term, TypeEnv},
    type_inference::InferenceError,
};
use crate::{
    ast::{Ast, AstNode, BinOp, Identifier, Pattern, Program, Symbol},
    interpreter::interpret::find_data_declarations,
};
use im::{hashmap, vector, HashMap};

#[derive(PartialEq, Debug, Clone, Hash)]
pub struct DataDeclTable {
    pub table: HashMap<String, (String, Vec<Identifier>)>,
}
impl DataDeclTable {
    pub fn new() -> Self {
        DataDeclTable {
            table: HashMap::new(),
        }
    }
    pub fn from_hashmap(table: HashMap<String, (String, Vec<Identifier>)>) -> Self {
        DataDeclTable { table }
    }
    pub fn get(&self, key: &String) -> Option<&(String, Vec<Identifier>)> {
        self.table.get(key)
    }
}

#[derive(PartialEq, Debug, Clone, Hash)]
pub struct InferenceContext<'a> {
    pub env: TypeEnv,
    pub func_table: &'a TypeEnv,
    pub data_decl_table: &'a DataDeclTable,
}
impl<'a> InferenceContext<'a> {
    pub fn new(env: TypeEnv, func_table: &'a TypeEnv, data_decl_table: &'a DataDeclTable) -> Self {
        InferenceContext {
            env,
            func_table,
            data_decl_table,
        }
    }
    pub fn new_env(&self, env: TypeEnv) -> Self {
        InferenceContext {
            env,
            func_table: self.func_table,
            data_decl_table: self.data_decl_table,
        }
    }
    pub fn update_env(&self, env: TypeEnv) -> Self {
        InferenceContext {
            env: self.env.clone().union(env),
            func_table: self.func_table,
            data_decl_table: self.data_decl_table,
        }
    }
}

pub fn generate_constraints(
    program: &Program,
    data_decl_table: &DataDeclTable,
) -> Result<ConstraintSet, InferenceError> {
    let data_funcs_ast = match find_data_declarations(&program) {
        Ok(v) => Ok(v),
        Err(e) => Err(InferenceError::DataDeclarationError(e)),
    }?;

    // Find functions and functions to make ADT literals
    let (user_funcs_constraints, user_funcs) = find_functions(&program)?;
    let (data_funcs_constraints, data_funcs) = find_functions(&data_funcs_ast)?;
    let func_table = user_funcs.union(data_funcs);

    let mut env: TypeEnv = HashMap::new();
    let mut constraint_set = ConstraintSet::new();
    constraint_set = constraint_set.union(user_funcs_constraints);
    constraint_set = constraint_set.union(data_funcs_constraints);

    for expr in program {
        let context = InferenceContext::new(env, &func_table, &data_decl_table);
        let (new_constraint_set, new_env) = generate_constraints_top_level(&expr, context)?;

        env = new_env;
        constraint_set = constraint_set.union(new_constraint_set);
    }

    Ok(constraint_set)
}

pub fn find_types(program: &Program) -> DataDeclTable {
    let mut table: im::HashMap<String, (String, Vec<Identifier>)> = hashmap![];
    for expr in program {
        match expr {
            Ast {
                node: AstNode::DataDeclarationNode(name, variants),
                ..
            } => {
                for (variant_name, id_decls) in variants {
                    table.insert(variant_name.clone(), (name.clone(), id_decls.clone()));
                }
            }
            _ => (),
        }
    }
    return DataDeclTable::from_hashmap(table);
}

fn find_functions(program: &Program) -> Result<(ConstraintSet, TypeEnv), InferenceError> {
    let mut env: TypeEnv = HashMap::new();
    let mut constraint_set = ConstraintSet::new();
    for expr in program {
        match &expr.node {
            AstNode::FunctionNode(name, params, return_type, body) => {
                // ensure the function has full type annotations
                let mut param_types = vector![];
                let mut param_type_constraints = ConstraintSet::new();
                for param in params {
                    match param.type_decl.clone() {
                        Some(t) => {
                            param_types.push_back(Term::from_type(&t));
                            param_type_constraints =
                                param_type_constraints.union(ConstraintSet::priority_unit(
                                    Term::Var(param.label),
                                    Term::from_type(&t),
                                ));
                        }
                        None => param_types.push_back(Term::new_var()),
                    }
                }
                let return_type_term = match return_type {
                    Some(t) => Term::from_type(t),
                    None => Term::new_var(),
                };

                let return_type_constraint =
                    ConstraintSet::priority_unit(Term::Var(body.label), return_type_term.clone());

                let expr_constraint = ConstraintSet::priority_unit(
                    Term::Var(expr.label),
                    Term::function(param_types, return_type_term),
                );

                // TODO: possibly more constraints here
                constraint_set = constraint_set
                    .union(param_type_constraints)
                    .union(return_type_constraint)
                    .union(expr_constraint);
                env.insert(name.clone(), expr.label);
            }
            _ => (),
        };
    }

    return Ok((constraint_set, env));
}

fn generate_constraints_top_level(
    expr: &Ast,
    context: InferenceContext,
) -> Result<(ConstraintSet, TypeEnv), InferenceError> {
    let InferenceContext { env, .. } = context.clone();
    match &expr.node {
        AstNode::LetNodeTopLevel(id, binding) => {
            let body_constraints = generate_constraint_expr(binding, context)?;
            let let_constraint = ConstraintSet::unit(Term::Var(id.label), Term::Var(binding.label));
            let type_annotation_constraint = if let Some(type_annotation) = &id.type_decl {
                ConstraintSet::unit(Term::Var(id.label), Term::from_type(type_annotation))
            } else {
                ConstraintSet::new()
            };

            let mut new_env = env.clone();
            new_env.insert(id.id.clone(), binding.label);

            Ok((
                body_constraints
                    .union(let_constraint)
                    .union(type_annotation_constraint),
                new_env,
            ))
        }
        AstNode::LetNode(_, _, _) => Err(InferenceError::TopLevelError(expr.src_loc.clone())),
        AstNode::FunctionNode(_, _, _, _) => Ok((ConstraintSet::new(), env)),
        AstNode::DataDeclarationNode(_, _) => Ok((ConstraintSet::new(), env)),
        _ => Ok((generate_constraint_expr(expr, context)?, env)),
    }
}

pub fn generate_constraint_expr(
    expr: &Ast,
    context: InferenceContext,
) -> Result<ConstraintSet, InferenceError> {
    let InferenceContext {
        env,
        func_table,
        data_decl_table,
    } = context.clone();
    match &expr.node {
        AstNode::NumberNode(_val) => Ok(ConstraintSet::unit(Term::Var(expr.label), Term::number())),
        AstNode::BoolNode(_val) => Ok(ConstraintSet::unit(Term::Var(expr.label), Term::boolean())),
        AstNode::VarNode(id) => {
            if let Some(id) = env.get(id) {
                Ok(ConstraintSet::unit(Term::Var(expr.label), Term::Var(*id)))
            } else if let Some(id) = func_table.get(id) {
                Ok(ConstraintSet::unit(Term::Var(expr.label), Term::Var(*id)))
            } else {
                Err(InferenceError::UnboundIdentifier(id.to_string(), env))
            }
        }
        AstNode::LetNodeTopLevel(_id, expr) => Err(InferenceError::TopLevelExpressionOutOfPlace(
            expr.src_loc.clone(),
        )),
        AstNode::LetNode(id, expr, body) => {
            let mut new_env = env.clone();
            new_env.insert(id.id.clone(), expr.label);
            let expr_constraints = generate_constraint_expr(&expr, context.clone())?;
            let body_constraints = generate_constraint_expr(&body, context.new_env(new_env))?;
            let type_annotation_constraint = if let Some(type_annotation) = &id.type_decl {
                ConstraintSet::unit(Term::Var(id.label), Term::from_type(type_annotation))
            } else {
                ConstraintSet::new()
            };
            Ok(expr_constraints
                .union(body_constraints)
                .union(type_annotation_constraint))
        }
        AstNode::IfNode(conditions_and_bodies, alternate) => {
            let mut first_term: Option<Term> = None;
            let mut constraints = ConstraintSet::new();
            for (condition, body) in conditions_and_bodies {
                constraints =
                    constraints.union(generate_constraint_expr(&condition, context.clone())?);
                constraints = constraints.union(generate_constraint_expr(&body, context.clone())?);
                constraints = constraints.union(ConstraintSet::unit(
                    Term::Var(condition.label),
                    Term::boolean(),
                ));
                if let Some(t) = &first_term {
                    constraints =
                        constraints.union(ConstraintSet::unit(t.clone(), Term::Var(body.label)))
                } else {
                    first_term = Some(Term::Var(body.label));
                }
            }

            constraints = constraints.union(generate_constraint_expr(&alternate, context)?);

            if let Some(v) = first_term {
                constraints = constraints.union(ConstraintSet::unit(v, Term::Var(alternate.label)));
            }

            // The overall expression must have same type as all branches
            constraints = constraints.union(ConstraintSet::unit(
                Term::Var(expr.label),
                Term::Var(alternate.label),
            ));

            Ok(constraints)
        }
        AstNode::BinOpNode(op, e1, e2) => {
            constraint_gen_binop(op.clone(), expr.label, &e1, &e2, context)
        }
        AstNode::FunCallNode(fun_value, arg_list) => {
            // 1. expressions type is a value
            // 2. functions type is a function
            let arg_terms = arg_list.iter().map(|arg| Term::Var(arg.label)).collect();
            let new_constraint = ConstraintSet::unit(
                Term::Var(fun_value.label),
                Term::function(arg_terms, Term::Var(expr.label)),
            );

            let mut arg_constraints = vec![];
            for arg in arg_list {
                arg_constraints.push(generate_constraint_expr(&arg, context.clone())?);
            }
            let arg_constraints = ConstraintSet::unions(arg_constraints);

            let fun_constraints = generate_constraint_expr(&fun_value, context)?;

            Ok(fun_constraints.union(new_constraint).union(arg_constraints))
        }
        AstNode::LambdaNode(param_list, body) => {
            let mut lam_env = env.clone();
            for param in param_list {
                lam_env.insert(param.id.clone(), param.label);
            }
            let body_constraints = generate_constraint_expr(&body, context.new_env(lam_env))?;

            let param_labels = param_list
                .iter()
                .map(|param| Term::Var(param.label))
                .collect();
            let param_constraints = ConstraintSet::unit(
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
        AstNode::DataLiteralNode(discriminant, _values) => Ok(ConstraintSet::unit(
            Term::Var(expr.label),
            Term::Constructor(discriminant.get_type().to_string(), vector![]),
        )),
        AstNode::MatchNode(expression_to_match, branches) => {
            let mut last_term: Option<Term> = None;
            let mut constraints = ConstraintSet::new();
            constraints = constraints.union(generate_constraint_expr(
                expression_to_match,
                context.clone(),
            )?);
            for (pattern, body) in branches {
                let pattern_env = get_identifiers_from_pattern(
                    expression_to_match.label,
                    pattern,
                    data_decl_table,
                )?;
                constraints = constraints.union(generate_constraint_expr(
                    &body,
                    context.update_env(pattern_env),
                )?);
                if let Some(v) = last_term {
                    constraints = constraints.union(ConstraintSet::unit(v, Term::Var(body.label)));

                    last_term = Some(Term::Var(body.label));
                }
            }

            Ok(constraints)
        }
    }
}

fn get_identifiers_from_pattern(
    target_label: Symbol,
    pattern: &Pattern,
    data_decl_table: &DataDeclTable,
) -> Result<TypeEnv, InferenceError> {
    match pattern {
        Pattern::Identifier(id) => Ok(hashmap![id.clone() => target_label]),
        pattern => Ok(get_identifiers_from_pattern_helper(
            pattern,
            data_decl_table,
        )?),
    }
}
fn get_identifiers_from_pattern_helper(
    pattern: &Pattern,
    data_decl_table: &DataDeclTable,
) -> Result<TypeEnv, InferenceError> {
    match pattern {
        Pattern::Data(name, patterns) => match data_decl_table.get(name) {
            Some((_type_name, identifier_decls)) => {
                if patterns.len() != identifier_decls.len() {
                    return Err(InferenceError::MalformedPattern(pattern.clone()));
                }
                let mut out = hashmap![];
                for (pattern, id_decl) in patterns.iter().zip(identifier_decls) {
                    match pattern {
                        Pattern::Identifier(id) => {
                            out.insert(id.clone(), id_decl.label);
                        }
                        pattern => {
                            out = out.union(get_identifiers_from_pattern_helper(
                                pattern,
                                data_decl_table,
                            )?);
                        }
                    }
                }
                return Ok(out);
            }
            None => Err(InferenceError::UnboundPattern(
                name.clone(),
                data_decl_table.clone(),
            )),
        },
        Pattern::Identifier(id) => panic!("Identifier found while typechecking pattern: {}", id),
        _ => Ok(hashmap![]),
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
    ConstraintSet::from_vec(vec![
        ConstraintSet::new_constraint(Term::Var(left_label), left_type),
        ConstraintSet::new_constraint(Term::Var(right_label), right_type),
        ConstraintSet::new_constraint(Term::Var(label), output_type),
    ])
}

fn constraint_gen_binop(
    op: BinOp,
    label: Symbol,
    e1: &Ast,
    e2: &Ast,
    context: InferenceContext,
) -> Result<ConstraintSet, InferenceError> {
    let c1 = generate_constraint_expr(e1, context.clone())?;
    let c2 = generate_constraint_expr(e2, context)?;

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
        BinOp::Eq => ConstraintSet::from_vec(vec![
            ConstraintSet::new_constraint(Term::Var(e1.label), Term::Var(e2.label)),
            ConstraintSet::new_constraint(Term::Var(label), Term::boolean()),
        ]),
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
