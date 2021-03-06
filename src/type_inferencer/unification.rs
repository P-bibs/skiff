use std::iter::once;

use crate::ast::Symbol;

use super::{
    ast::{Constraint, ConstraintSet, SubstitutionSet, Term},
    type_inference::InferenceError,
};
use im::HashMap;

pub fn unify_constraints(constraint_set: ConstraintSet) -> Result<SubstitutionSet, InferenceError> {
    let mut constraint_set: Vec<Constraint> = constraint_set.into_vec();
    let mut substitution_set: SubstitutionSet = HashMap::new();

    loop {
        match constraint_set.pop() {
            Some(constraint) => {
                let (left, right) = constraint;
                match left {
                    Term::Var(l) => {
                        if left != right {
                            if occurs_check(&left, &right) {
                                return Err(InferenceError::InfiniteType());
                            }
                            constraint_set = replace_in_constraints(l, &right, constraint_set);
                            substitution_set =
                                replace_in_substitutions(l, &right, substitution_set);
                        }
                    }
                    Term::Constructor(head1, args1) => match right {
                        Term::Var(r) => {
                            constraint_set.push((Term::Var(r), Term::Constructor(head1, args1)));
                        }
                        Term::Constructor(head2, args2) => {
                            // If either type is any then the type check automatically passes
                            if head1 == "Any" || head2 == "Any" {
                                continue;
                            }
                            if head1 == head2 {
                                constraint_set.extend(args1.into_iter().zip(args2))
                            } else {
                                return Err(InferenceError::ConstructorMismatch(
                                    Term::Constructor(head1, args1),
                                    Term::Constructor(head2, args2),
                                ));
                            }
                        }
                    },
                }
            }
            None => break,
        }
    }
    Ok(substitution_set)
}

fn replace_in_constraints(
    replace: Symbol,
    with: &Term,
    constraint_set: Vec<Constraint>,
) -> Vec<Constraint> {
    constraint_set
        .into_iter()
        .map(|(left, right)| {
            (
                replace_in_term(replace, with, left),
                replace_in_term(replace, with, right),
            )
        })
        .collect()
}

fn replace_in_term(replace: Symbol, with: &Term, target: Term) -> Term {
    if target == Term::any() {
        target
    } else {
        match target {
            Term::Var(label) => {
                if label == replace {
                    with.clone()
                } else {
                    target
                }
            }
            Term::Constructor(head, args) => Term::Constructor(
                head,
                args.into_iter()
                    .map(|arg| replace_in_term(replace, with, arg))
                    .collect(),
            ),
        }
    }
}

fn replace_in_substitutions(
    replace: Symbol,
    with: &Term,
    substitutions: SubstitutionSet,
) -> SubstitutionSet {
    substitutions
        .into_iter()
        .map(|(key, value)| (key, replace_in_term(replace, with, value)))
        .chain(once((replace, with.clone())))
        .collect()
}

fn occurs_check(replace: &Term, with: &Term) -> bool {
    *replace == *with
        || match with {
            Term::Var(_) => false,
            Term::Constructor(_head, args) => {
                args.into_iter().any(|arg| occurs_check(replace, arg))
            }
        }
}
