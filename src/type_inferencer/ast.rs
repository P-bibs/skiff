use crate::ast::{Symbol, Type};
use im::{HashMap, HashSet, Vector};

pub type TypeEnv = HashMap<String, Symbol>;

pub type ConstraintSet = HashSet<Constraint>;
pub type Constraint = (Term, Term);

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
    pub fn from_type(t: &Type) -> Term {
        Term::Constructor(
            t.id.clone(),
            t.args.iter().map(|t| Term::from_type(t)).collect(),
        )
    }
}
