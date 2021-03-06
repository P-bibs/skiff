use super::{
    ast::{SubstitutionSet, Term, TypeEnv},
    constraint_gen::{generate_constraints, DataDeclTable},
    unification::unify_constraints,
};
use crate::{
    ast::{Pattern, Program, SrcLoc},
    interpreter::interpret::InterpError,
};
use std::ops::Range;

#[derive(PartialEq, Debug, Clone, Hash)]
pub enum InferenceError {
    UnboundIdentifier(String, TypeEnv),
    UnboundPattern(String, DataDeclTable),
    MalformedPattern(Pattern),
    ConstructorMismatch(Term, Term),
    InfiniteType(),
    MissingAnnotation(Range<usize>),
    TopLevelError(SrcLoc),
    TopLevelExpressionOutOfPlace(SrcLoc),
    DataDeclarationError(InterpError),
}

pub fn infer_types(
    program: &Program,
    data_decl_table: &DataDeclTable,
) -> Result<SubstitutionSet, InferenceError> {
    // println!("PROGRAM: {:?}", program);
    let constraint_set = generate_constraints(&program, data_decl_table)?;
    // println!("CONSTRAINTS: {:?}", constraint_set);
    let substition_set = unify_constraints(constraint_set)?;
    Ok(substition_set)
}
