use super::{
    ast::{ConstraintSet, SubstitutionSet},
    type_inference::InferenceError,
};
use im::HashMap;

pub fn unify_constraints(constraint_set: ConstraintSet) -> Result<SubstitutionSet, InferenceError> {
    Ok(HashMap::new())
}
