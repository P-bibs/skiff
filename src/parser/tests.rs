#[cfg(test)]
mod parse_expr_tests {
    use super::{parse_expr, Ast, BinOp, Token};

    #[test]
    fn parses_number() {
        let input: &mut Vec<Token> = &mut vec![Token::Number(1)];
        input.reverse();

        let result = parse_expr(input, 0, false).unwrap();
        let expected_output = Ast::NumberNode(1);
        assert_eq!(result, expected_output);
    }

    #[test]
    fn parses_numbers_and_operators_with_precedence() {
        let input: &mut Vec<Token> = &mut vec![
            Token::Number(1),
            Token::Plus,
            Token::Number(2),
            Token::Times,
            Token::Number(3),
        ];
        input.reverse();

        let result = parse_expr(input, 0, false).unwrap();
        let expected_output = Ast::BinOpNode(
            BinOp::Plus,
            Box::new(Ast::NumberNode(1)),
            Box::new(Ast::BinOpNode(
                BinOp::Times,
                Box::new(Ast::NumberNode(2)),
                Box::new(Ast::NumberNode(3)),
            )),
        );
        assert_eq!(result, expected_output);
    }

    #[test]
    fn parses_parenthesis_to_override_precedence() {
        let input: &mut Vec<Token> = &mut vec![
            Token::LParen,
            Token::Number(1),
            Token::Plus,
            Token::Number(2),
            Token::RParen,
            Token::Times,
            Token::Number(3),
        ];
        input.reverse();

        let result = parse_expr(input, 0, false).unwrap();
        let expected_output = Ast::BinOpNode(
            BinOp::Times,
            Box::new(Ast::BinOpNode(
                BinOp::Plus,
                Box::new(Ast::NumberNode(1)),
                Box::new(Ast::NumberNode(2)),
            )),
            Box::new(Ast::NumberNode(3)),
        );
        assert_eq!(result, expected_output);
    }

    #[test]
    fn parses_function_calls() {
        let input: &mut Vec<Token> = &mut vec![
            Token::Identifier("f".to_string()),
            Token::LParen,
            Token::Number(2),
            Token::Comma,
            Token::Number(3),
            Token::RParen,
        ];
        input.reverse();

        let result = parse_expr(input, 0, false);
        let expected_output = Ok(Ast::FunCallNode(
            Box::new(Ast::VarNode("f".to_string())),
            vec![Ast::NumberNode(2), Ast::NumberNode(3)],
        ));

        assert_eq!(result, expected_output);
    }

    #[test]
    fn parses_identifiers() {
        let input: &mut Vec<Token> = &mut vec![Token::Identifier("f".to_string())];
        input.reverse();

        let result = parse_expr(input, 0, false);
        let expected_output = Ok(Ast::VarNode("f".to_string()));

        assert_eq!(result, expected_output);
    }
}

#[cfg(test)]
mod parse_arg_tests {
    use super::{parse_args, Ast, BinOp, Token};

    #[test]
    fn parses_no_args() {
        let input: &mut Vec<Token> = &mut vec![Token::RParen];
        input.reverse();

        let result = parse_args(input);
        let expected_output = Ok(vec![]);
        assert_eq!(result, expected_output);
    }

    #[test]
    fn parses_one_simple_arg() {
        let input: &mut Vec<Token> = &mut vec![Token::Number(1), Token::RParen];
        input.reverse();

        let result = parse_args(input);
        let expected_output = Ok(vec![Ast::NumberNode(1)]);
        assert_eq!(result, expected_output);
    }

    #[test]
    fn parses_two_simple_args() {
        let input: &mut Vec<Token> = &mut vec![
            Token::Number(1),
            Token::Comma,
            Token::Number(2),
            Token::RParen,
        ];
        input.reverse();

        let result = parse_args(input);
        let expected_output = Ok(vec![Ast::NumberNode(1), Ast::NumberNode(2)]);
        assert_eq!(result, expected_output);
    }

    #[test]
    fn parses_two_complex_args() {
        let input: &mut Vec<Token> = &mut vec![
            Token::Number(1),
            Token::Plus,
            Token::Number(1),
            Token::Comma,
            Token::Number(2),
            Token::Times,
            Token::Number(1),
            Token::RParen,
        ];
        input.reverse();

        let result = parse_args(input);
        let expected_output = Ok(vec![
            Ast::BinOpNode(
                BinOp::Plus,
                Box::new(Ast::NumberNode(1)),
                Box::new(Ast::NumberNode(1)),
            ),
            Ast::BinOpNode(
                BinOp::Times,
                Box::new(Ast::NumberNode(2)),
                Box::new(Ast::NumberNode(1)),
            ),
        ]);
        assert_eq!(result, expected_output);
    }
}

#[cfg(test)]
mod parse_params_tests {
    use super::{parse_params, ParseError, Token};

    fn test_program(
        input: &mut Vec<Token>,
        expected_output: Result<Vec<String>, ParseError>,
    ) -> () {
        input.reverse();

        let result = parse_params(input);
        assert_eq!(result, expected_output);
    }

    #[test]
    fn parses_no_params() {
        let input: &mut Vec<Token> = &mut vec![Token::RParen];

        let expected_output = Ok(vec![]);

        test_program(input, expected_output);
    }

    #[test]
    fn parses_one_param() {
        let input: &mut Vec<Token> = &mut vec![Token::Identifier("f".to_string()), Token::RParen];

        let expected_output = Ok(vec!["f".to_string()]);

        test_program(input, expected_output);
    }

    #[test]
    fn parses_two_params() {
        let input: &mut Vec<Token> = &mut vec![
            Token::Identifier("a".to_string()),
            Token::Comma,
            Token::Identifier("b".to_string()),
            Token::RParen,
        ];

        let expected_output = Ok(vec!["a".to_string(), "b".to_string()]);

        test_program(input, expected_output);
    }
}

#[cfg(test)]
mod parse_program_tests {
    use super::{parse_program, Ast, BinOp, ParseError, Program, Token};

    fn test_program(input: &mut Vec<Token>, expected_output: Result<Program, ParseError>) -> () {
        input.reverse();

        let result = parse_program(input);
        assert_eq!(result, expected_output);
    }

    #[test]
    fn parses_no_exprs() {
        let input: &mut Vec<Token> = &mut vec![];

        let expected_output: Result<Program, ParseError> = Ok(vec![]);

        test_program(input, expected_output);
    }

    #[test]
    fn parses_one_simple_expr() {
        let input: &mut Vec<Token> = &mut vec![Token::Number(1)];

        let expected_output: Result<Program, ParseError> = Ok(vec![Ast::NumberNode(1)]);

        test_program(input, expected_output);
    }

    #[test]
    fn parses_two_simple_exprs() {
        let input: &mut Vec<Token> = &mut vec![Token::Number(1), Token::Number(2)];

        let expected_output: Result<Program, ParseError> =
            Ok(vec![Ast::NumberNode(1), Ast::NumberNode(2)]);

        test_program(input, expected_output);
    }

    #[test]
    fn parses_one_complex_expr() {
        let input: &mut Vec<Token> = &mut vec![
            Token::Number(1),
            Token::Plus,
            Token::Number(2),
            Token::Times,
            Token::Number(3),
        ];

        let expected_output: Result<Program, ParseError> = Ok(vec![Ast::BinOpNode(
            BinOp::Plus,
            Box::new(Ast::NumberNode(1)),
            Box::new(Ast::BinOpNode(
                BinOp::Times,
                Box::new(Ast::NumberNode(2)),
                Box::new(Ast::NumberNode(3)),
            )),
        )]);

        test_program(input, expected_output);
    }

    #[test]
    fn parses_two_complex_exprs() {
        let input: &mut Vec<Token> = &mut vec![
            Token::Number(1),
            Token::Plus,
            Token::Number(2),
            Token::Times,
            Token::Number(3),
            Token::Number(4),
            Token::Plus,
            Token::Number(5),
            Token::Times,
            Token::Number(6),
        ];

        let expected_output: Result<Program, ParseError> = Ok(vec![
            Ast::BinOpNode(
                BinOp::Plus,
                Box::new(Ast::NumberNode(1)),
                Box::new(Ast::BinOpNode(
                    BinOp::Times,
                    Box::new(Ast::NumberNode(2)),
                    Box::new(Ast::NumberNode(3)),
                )),
            ),
            Ast::BinOpNode(
                BinOp::Plus,
                Box::new(Ast::NumberNode(4)),
                Box::new(Ast::BinOpNode(
                    BinOp::Times,
                    Box::new(Ast::NumberNode(5)),
                    Box::new(Ast::NumberNode(6)),
                )),
            ),
        ]);

        test_program(input, expected_output);
    }
}
