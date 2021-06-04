use skiff::ast::Val;
use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq)]
pub enum SimpleVal {
    Num(i64),
    Bool(bool),
    Lam(),
    Data(String, Vec<SimpleVal>),
}

impl<'a> SimpleVal {
    pub fn new(val: &Val) -> SimpleVal {
        match val {
            Val::Num(n) => SimpleVal::Num(*n),
            Val::Bool(b) => SimpleVal::Bool(*b),
            Val::Lam(_, _, _) => SimpleVal::Lam(),
            Val::Data(discriminant, fields) => SimpleVal::Data(
                discriminant.clone(),
                fields.iter().map(|x| SimpleVal::new(x)).collect(),
            ),
        }
    }
}

pub fn get_expected_output<'a>() -> HashMap<&'a str, Vec<SimpleVal>> {
    let map: HashMap<&str, Vec<SimpleVal>> = [
        ("algebraic_double_lam.boat", vec![SimpleVal::Num(10)]),
        (
            "exprs_and_defns.boat",
            vec![SimpleVal::Num(10), SimpleVal::Num(20)],
        ),
        ("fib.boat", vec![SimpleVal::Num(6765)]),
        ("function_keyword.boat", vec![SimpleVal::Num(6)]),
        ("function_recursive.boat", vec![SimpleVal::Num(15)]),
        ("identity_lam.boat", vec![SimpleVal::Num(1)]),
        ("if_double_if_one.boat", vec![SimpleVal::Num(3)]),
        ("if_elif.boat", vec![SimpleVal::Num(3)]),
        ("if_elif_else.boat", vec![SimpleVal::Num(4)]),
        ("let_recursive_shadow.boat", vec![SimpleVal::Num(1)]),
        ("let_with_no_exprs.boat", vec![]),
        (
            "let_with_simple_exprs.boat",
            vec![SimpleVal::Num(7), SimpleVal::Num(15), SimpleVal::Num(6)],
        ),
        ("paren_overrides_precedence.boat", vec![SimpleVal::Num(9)]),
        ("plus_and_times_precedence.boat", vec![SimpleVal::Num(7)]),
        ("simple_bool.boat", vec![SimpleVal::Bool(false)]),
        ("simple_hof.skf", vec![SimpleVal::Num(3)]),
        ("simple_if.boat", vec![SimpleVal::Num(1)]),
        (
            "adt_simple.boat",
            vec![
                SimpleVal::Data("None".to_string(), vec![]),
                SimpleVal::Data("Some".to_string(), vec![SimpleVal::Num(1)]),
            ],
        ),
        (
            "binops.boat",
            vec![
                SimpleVal::Num(9),
                SimpleVal::Num(9),
                SimpleVal::Num(9),
                SimpleVal::Num(9),
                SimpleVal::Num(9),
                SimpleVal::Num(9),
                SimpleVal::Bool(true),
                SimpleVal::Bool(true),
                SimpleVal::Bool(true),
                SimpleVal::Bool(true),
                SimpleVal::Bool(true),
                SimpleVal::Bool(true),
                SimpleVal::Bool(true),
                SimpleVal::Num(9),
                SimpleVal::Num(9),
                SimpleVal::Num(9),
            ],
        ),
        ("pattern_match_simple.boat", vec![SimpleVal::Num(2)]),
    ]
    .iter()
    .cloned()
    .collect();

    return map;
}
