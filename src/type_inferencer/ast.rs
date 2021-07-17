use std::fmt::{self, Display};

use crate::ast::{gensym, Symbol, Type};
use im::{HashMap, HashSet, Vector};

pub type TypeEnv = HashMap<String, Symbol>;

pub type Constraint = (Term, Term);
#[derive(Eq, PartialEq, Debug, Clone, Hash)]
pub struct ConstraintSet {
    set: HashSet<((Term, Term), usize)>,
}
impl ConstraintSet {
    pub fn new() -> Self {
        ConstraintSet {
            set: HashSet::new(),
        }
    }
    pub fn new_constraint(t1: Term, t2: Term) -> Constraint {
        (t1, t2)
    }
    pub fn unit(t1: Term, t2: Term) -> Self {
        ConstraintSet {
            set: HashSet::unit(((t1, t2), 0)),
        }
    }
    pub fn priority_unit(t1: Term, t2: Term) -> Self {
        ConstraintSet {
            set: HashSet::unit(((t1, t2), 1)),
        }
    }
    pub fn union(self, other: Self) -> Self {
        ConstraintSet {
            set: self.set.union(other.set),
        }
    }
    pub fn from_vec(vec: Vec<Constraint>) -> Self {
        ConstraintSet {
            set: (vec.into_iter().map(|c| (c, 0)).collect()),
        }
    }
    pub fn into_vec(self) -> Vec<Constraint> {
        let mut vec: Vec<((Term, Term), usize)> = self.set.into_iter().collect();
        vec.sort_by(|(_, x), (_, y)| x.cmp(y));
        vec.into_iter().map(|(c, _)| c).collect()
    }
    pub fn unions<I>(i: I) -> Self
    where
        I: IntoIterator<Item = Self>,
    {
        ConstraintSet {
            set: HashSet::unions(i.into_iter().map(|i| i.set)),
        }
    }
}

pub type SubstitutionSet = HashMap<Symbol, Term>;

#[derive(Eq, PartialEq, Debug, Clone, Hash)]
pub enum Term {
    Var(Symbol),
    Constructor(String, Vector<Term>),
}
impl Term {
    pub fn number() -> Term {
        Term::Constructor("Number".to_string(), Vector::new())
    }
    pub fn boolean() -> Term {
        Term::Constructor("Boolean".to_string(), Vector::new())
    }
    pub fn function(args: Vector<Term>, return_type: Term) -> Term {
        let mut v = args.clone();
        v.push_back(return_type);
        Term::Constructor("Function".to_string(), v)
    }
    pub fn any() -> Term {
        Term::Constructor("Any".to_string(), Vector::new())
    }
    pub fn from_type(t: &Type) -> Term {
        Term::Constructor(
            t.id.clone(),
            t.args.iter().map(|t| Term::from_type(t)).collect(),
        )
    }
    pub fn new_var() -> Self {
        Term::Var(gensym())
    }
}
impl Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Term::Var(label) => {
                write!(f, "{}", label)
            }
            Term::Constructor(id, args) => {
                if args.len() == 0 {
                    write!(f, "{}", id)
                } else {
                    write!(
                        f,
                        "{}<{}>",
                        id,
                        args.iter()
                            .map(|arg| format!("{}", arg))
                            .collect::<Vec<String>>()
                            .join(", ")
                    )
                }
            }
        }
    }
}
