use super::{
    ast::{SubstitutionSet, Term},
    constraint_gen::generate_constraints,
    unification::unify_constraints,
};
use crate::{
    ast::{Program, SrcLoc},
    interpreter::interpret::InterpError,
};
use std::ops::Range;

#[derive(PartialEq, Debug, Clone, Hash)]
pub enum InferenceError {
    UnboundIdentifier(String),
    ConstructorMismatch(Term, Term),
    InfiniteType(),
    MissingAnnotation(Range<usize>),
    TopLevelError(SrcLoc),
    TopLevelExpressionOutOfPlace(SrcLoc),
    DataDeclarationError(InterpError),
}

pub fn infer_types(program: &Program) -> Result<SubstitutionSet, InferenceError> {
    // println!("PROGRAM: {:?}", program);
    let constraint_set = generate_constraints(&program)?;
    // println!("CONSTRAINTS: {:?}", constraint_set);
    let substition_set = unify_constraints(constraint_set)?;
    Ok(substition_set)
}
