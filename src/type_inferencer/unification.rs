use std::iter::once;

use crate::ast::Symbol;

use super::{
    ast::{Constraint, ConstraintSet, SubstitutionSet, Term},
    type_inference::InferenceError,
};
use im::HashMap;

pub fn unify_constraints(constraint_set: ConstraintSet) -> Result<SubstitutionSet, InferenceError> {
    let mut constraint_set: Vec<Constraint> = constraint_set.iter().cloned().collect();

    let mut substitution_set: SubstitutionSet = HashMap::new();

    loop {
        println!("con set {:?}", constraint_set);
        println!("sub set {:?}", substitution_set);
        match constraint_set.pop() {
            Some(constraint) => {
                let (left, right) = constraint;
                match left {
                    Term::Var(l) => {
                        if left != right {
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
                            if head1 == head2 {
                                constraint_set.extend(args1.into_iter().zip(args2))

                            // for (arg1, arg2) in args1.into_iter().zip(args2) {
                            //     constraint_set.push((arg1.clone(), arg2.clone()));
                            // }
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
            Term::Var(l) => false,
            Term::Constructor(head, args) => args.into_iter().any(|arg| occurs_check(replace, arg)),
        }
}
