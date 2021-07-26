use std::collections::HashMap;

use crate::ast::{Pattern, Type, Val};

#[derive(PartialEq, Debug, Clone, Hash)]
pub enum ExhaustivenessError {
    UnknownTypeToMatchOn(),
    UnknownTypeVariant(),
    CantMatchFunction(),
    CantMatchAny(),
    NotEnoughArgsInPattern(),
    TooManyArgsInPattern(),
}

type TypeTable = HashMap<String, Vec<(String, Vec<Type>)>>;

pub fn check_pattern_exhaustiveness<'a>(
    target_type: &Type,
    patterns: &Vec<Pattern>,
    type_table: &TypeTable,
) -> Result<bool, ExhaustivenessError> {
    match target_type {
        Type { id, .. } if id == "Number" => {
            if patterns.iter().any(|x| x.is_identifier()) {
                Ok(true)
            } else {
                Ok(false)
            }
        }
        Type { id, .. } if id == "Boolean" => {
            if patterns.iter().any(|x| x.is_identifier())
                || (patterns.iter().any(|x| *x == Pattern::BoolLiteral(true))
                    && patterns.iter().any(|x| *x == Pattern::BoolLiteral(false)))
            {
                Ok(true)
            } else {
                Ok(false)
            }
        }
        Type { id, .. } if id == "Function" => Err(ExhaustivenessError::CantMatchFunction()),
        Type { id, .. } if id == "Any" => Err(ExhaustivenessError::CantMatchAny()),
        Type { id, .. } => {
            if patterns.iter().any(|x| x.is_identifier()) {
                Ok(true)
            } else {
                if let Some(variants) = type_table.get(id) {
                    for (variant_name, variant_types) in variants {
                        let args_of_valid_patterns: Vec<&Vec<Pattern>> = patterns
                            .iter()
                            .filter_map(|pattern| match pattern {
                                Pattern::Data(name, pattern_args) => {
                                    if name == variant_name {
                                        Some(pattern_args)
                                    } else {
                                        None
                                    }
                                }
                                _ => None,
                            })
                            .collect();

                        // If there aren't any patterns that match this variant then the match is not exhaustive
                        if args_of_valid_patterns.len() == 0 {
                            return Ok(false);
                        }

                        for (i, type_arg) in variant_types.iter().enumerate() {
                            let args_for_this_match: Vec<&Pattern> = args_of_valid_patterns
                                .iter()
                                .map(|pattern_args| match pattern_args.get(i) {
                                    Some(v) => Ok(v),
                                    None => Err(ExhaustivenessError::NotEnoughArgsInPattern()),
                                })
                                .collect::<Result<Vec<&Pattern>, ExhaustivenessError>>()?;

                            if check_pattern_exhaustiveness(
                                type_arg,
                                &args_for_this_match.into_iter().cloned().collect(),
                                type_table,
                            )? {
                                continue;
                            } else {
                                return Ok(false);
                            }
                        }
                    }
                    return Ok(true);
                } else {
                    Err(ExhaustivenessError::UnknownTypeToMatchOn())
                }
            }
        }
    }
}

#[cfg(test)]
mod exhaustiveness_tests {
    use im::vector;

    use super::*;

    #[test]
    fn fails_baseline_number_without_identifier() {
        let input_type = Type::new_number();
        let input_patterns: Vec<Pattern> = vec![Pattern::NumLiteral(1), Pattern::NumLiteral(2)];

        let result = check_pattern_exhaustiveness(&input_type, &input_patterns, &HashMap::new());
        let expected_output = Ok(false);
        assert_eq!(result, expected_output);
    }

    #[test]
    fn passes_baseline_number_with_identifier() {
        let input_type = Type::new_number();
        let input_patterns: Vec<Pattern> = vec![
            Pattern::NumLiteral(1),
            Pattern::NumLiteral(2),
            Pattern::Identifier("foo".to_string()),
        ];

        let result = check_pattern_exhaustiveness(&input_type, &input_patterns, &HashMap::new());
        let expected_output = Ok(true);
        assert_eq!(result, expected_output);
    }

    #[test]
    fn fails_baseline_boolean() {
        let input_type = Type::new_boolean();
        let input_patterns: Vec<Pattern> = vec![Pattern::BoolLiteral(true)];

        let result = check_pattern_exhaustiveness(&input_type, &input_patterns, &HashMap::new());
        let expected_output = Ok(false);
        assert_eq!(result, expected_output);
    }

    #[test]
    fn passes_baseline_boolean_without_identifier() {
        let input_type = Type::new_boolean();
        let input_patterns: Vec<Pattern> =
            vec![Pattern::BoolLiteral(true), Pattern::BoolLiteral(false)];

        let result = check_pattern_exhaustiveness(&input_type, &input_patterns, &HashMap::new());
        let expected_output = Ok(true);
        assert_eq!(result, expected_output);
    }

    #[test]
    fn passes_baseline_boolean_with_identifier() {
        let input_type = Type::new_boolean();
        let input_patterns: Vec<Pattern> = vec![Pattern::Identifier("_".to_string())];

        let result = check_pattern_exhaustiveness(&input_type, &input_patterns, &HashMap::new());
        let expected_output = Ok(true);
        assert_eq!(result, expected_output);
    }

    #[test]
    fn fails_option_type_if_no_none() {
        let input_type = Type::new("Option".to_string(), vector![]);
        let input_patterns: Vec<Pattern> = vec![
            Pattern::Data("some".to_string(), vec![Pattern::NumLiteral(10)]),
            Pattern::Data("some".to_string(), vec![Pattern::NumLiteral(20)]),
            Pattern::Data(
                "some".to_string(),
                vec![Pattern::Identifier("_".to_string())],
            ),
        ];
        let type_table: TypeTable = vec![(
            "Option".to_string(),
            vec![
                ("some".to_string(), vec![Type::new_number()]),
                ("none".to_string(), vec![]),
            ],
        )]
        .into_iter()
        .collect();

        let result = check_pattern_exhaustiveness(&input_type, &input_patterns, &type_table);
        let expected_output = Ok(false);
        assert_eq!(result, expected_output);
    }

    #[test]
    fn errors_on_not_enough_pattern_args_for_option() {
        let input_type = Type::new("Option".to_string(), vector![]);
        let input_patterns: Vec<Pattern> = vec![Pattern::Data("some".to_string(), vec![])];
        let type_table: TypeTable = vec![(
            "Option".to_string(),
            vec![
                ("some".to_string(), vec![Type::new_number()]),
                ("none".to_string(), vec![]),
            ],
        )]
        .into_iter()
        .collect();

        let result = check_pattern_exhaustiveness(&input_type, &input_patterns, &type_table);
        let expected_output = Err(ExhaustivenessError::NotEnoughArgsInPattern());
        assert_eq!(result, expected_output);
    }

    #[test]
    fn passes_option_type_with_wildcard_identifier() {
        let input_type = Type::new("Option".to_string(), vector![]);
        let input_patterns: Vec<Pattern> = vec![Pattern::Identifier("_".to_string())];
        let type_table: TypeTable = vec![(
            "Option".to_string(),
            vec![
                ("some".to_string(), vec![Type::new_number()]),
                ("none".to_string(), vec![]),
            ],
        )]
        .into_iter()
        .collect();

        let result = check_pattern_exhaustiveness(&input_type, &input_patterns, &type_table);
        let expected_output = Ok(true);
        assert_eq!(result, expected_output);
    }

    #[test]
    fn fails_option_type_if_no_identifier() {
        let input_type = Type::new("Option".to_string(), vector![]);
        let input_patterns: Vec<Pattern> = vec![
            Pattern::Data("some".to_string(), vec![Pattern::NumLiteral(10)]),
            Pattern::Data("some".to_string(), vec![Pattern::NumLiteral(20)]),
            Pattern::Data("none".to_string(), vec![Pattern::BoolLiteral(false)]),
        ];
        let type_table: TypeTable = vec![(
            "Option".to_string(),
            vec![
                ("some".to_string(), vec![Type::new_number()]),
                ("none".to_string(), vec![]),
            ],
        )]
        .into_iter()
        .collect();

        let result = check_pattern_exhaustiveness(&input_type, &input_patterns, &type_table);
        let expected_output = Ok(false);
        assert_eq!(result, expected_output);
    }

    #[test]
    fn passes_option_type_with_identifier() {
        let input_type = Type::new("Option".to_string(), vector![]);
        let input_patterns: Vec<Pattern> = vec![
            Pattern::Data("some".to_string(), vec![Pattern::NumLiteral(10)]),
            Pattern::Data("some".to_string(), vec![Pattern::NumLiteral(20)]),
            Pattern::Data(
                "some".to_string(),
                vec![Pattern::Identifier("_".to_string())],
            ),
            Pattern::Data("none".to_string(), vec![Pattern::BoolLiteral(false)]),
        ];
        let type_table: TypeTable = vec![(
            "Option".to_string(),
            vec![
                ("some".to_string(), vec![Type::new_number()]),
                ("none".to_string(), vec![]),
            ],
        )]
        .into_iter()
        .collect();

        let result = check_pattern_exhaustiveness(&input_type, &input_patterns, &type_table);
        let expected_output = Ok(true);
        assert_eq!(result, expected_output);
    }

    #[test]
    fn fails_complex_bool_nesting() {
        let input_type = Type::new("MaybeBools".to_string(), vector![]);
        let input_patterns: Vec<Pattern> = vec![
            Pattern::Data("one".to_string(), vec![Pattern::BoolLiteral(true)]),
            Pattern::Data("one".to_string(), vec![Pattern::BoolLiteral(false)]),
            Pattern::Data(
                "two".to_string(),
                vec![Pattern::BoolLiteral(false), Pattern::BoolLiteral(false)],
            ),
            Pattern::Data(
                "two".to_string(),
                vec![Pattern::BoolLiteral(true), Pattern::BoolLiteral(false)],
            ),
        ];
        let type_table: TypeTable = vec![(
            "MaybeBools".to_string(),
            vec![
                ("one".to_string(), vec![Type::new_boolean()]),
                (
                    "two".to_string(),
                    vec![Type::new_boolean(), Type::new_boolean()],
                ),
            ],
        )]
        .into_iter()
        .collect();

        let result = check_pattern_exhaustiveness(&input_type, &input_patterns, &type_table);
        let expected_output = Ok(false);
        assert_eq!(result, expected_output);
    }

    #[test]
    fn passes_complex_bool_nesting() {
        let input_type = Type::new("MaybeBools".to_string(), vector![]);
        let input_patterns: Vec<Pattern> = vec![
            Pattern::Data("one".to_string(), vec![Pattern::BoolLiteral(true)]),
            Pattern::Data("one".to_string(), vec![Pattern::BoolLiteral(false)]),
            Pattern::Data(
                "two".to_string(),
                vec![Pattern::BoolLiteral(false), Pattern::BoolLiteral(false)],
            ),
            Pattern::Data(
                "two".to_string(),
                vec![
                    Pattern::BoolLiteral(true),
                    Pattern::Identifier("_".to_string()),
                ],
            ),
        ];
        let type_table: TypeTable = vec![(
            "MaybeBools".to_string(),
            vec![
                ("one".to_string(), vec![Type::new_boolean()]),
                (
                    "two".to_string(),
                    vec![Type::new_boolean(), Type::new_boolean()],
                ),
            ],
        )]
        .into_iter()
        .collect();

        let result = check_pattern_exhaustiveness(&input_type, &input_patterns, &type_table);
        let expected_output = Ok(true);
        assert_eq!(result, expected_output);
    }
}
