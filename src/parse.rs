use crate::lexer::Token;

#[derive(PartialEq, Debug)]
pub enum Ast {
    NumberNode(i64),
    OperatorNode(Op, Box<Ast>, Box<Ast>),
}
#[derive(PartialEq, Debug, Clone, Copy)]
pub enum Op {
    Plus,
    Times,
}
#[derive(PartialEq, Debug)]
pub struct ParseError(String);

trait InitialParselet {
    fn parse(&self, tokens: &mut Vec<Token>, current_token: Token) -> Result<Ast, ParseError>;
}
trait ConsequentParselet {
    fn parse(
        &self,
        tokens: &mut Vec<Token>,
        left_node: Ast,
        current_token: Token,
    ) -> Result<Ast, ParseError>;
}

struct OperatorParselet {
    operator: Op,
    is_left_associative: bool,
}
impl ConsequentParselet for OperatorParselet {
    fn parse(
        &self,
        tokens: &mut Vec<Token>,
        left_node: Ast,
        _current_token: Token,
    ) -> Result<Ast, ParseError> {
        let my_binding_power = get_binding_power(self.operator);
        let right_node = parse(
            tokens,
            if self.is_left_associative {
                my_binding_power
            } else {
                my_binding_power - 1
            },
        )?;

        return Ok(Ast::OperatorNode(
            self.operator,
            Box::new(left_node),
            Box::new(right_node),
        ));
    }
}

struct NumberParselet {}
impl InitialParselet for NumberParselet {
    fn parse(&self, _tokens: &mut Vec<Token>, current_token: Token) -> Result<Ast, ParseError> {
        match current_token {
            Token::Number(n) => Ok(Ast::NumberNode(n)),
            _ => panic!("Tried to use number parselet with non-number token"),
        }
    }
}

struct ParenthesisParselet {}
impl InitialParselet for ParenthesisParselet {
    fn parse(&self, _tokens: &mut Vec<Token>, _current_token: Token) -> Result<Ast, ParseError> {
        panic!("Not yet implemented")
    }
}

fn get_binding_power(op: Op) -> i64 {
    match op {
        Op::Plus => 10,
        Op::Times => 20,
    }
}

fn token_op_to_ast_op(tok: &Token) -> Op {
    match tok {
        Token::Times => Op::Times,
        Token::Plus => Op::Plus,
        _ => panic!("This token does not correspond to an operator"),
    }
}

fn initial_map(tok: &Token) -> Option<Box<dyn InitialParselet>> {
    match *tok {
        Token::Number(_) => Some(Box::new(NumberParselet {})),
        Token::LParen => Some(Box::new(ParenthesisParselet {})),
        _ => panic!("Not yet implemented"),
    }
}

fn consequent_map(tok: &Token) -> Option<Box<dyn ConsequentParselet>> {
    match *tok {
        Token::Plus => Some(Box::new(OperatorParselet {
            operator: Op::Plus,
            is_left_associative: true,
        })),
        Token::Times => Some(Box::new(OperatorParselet {
            operator: Op::Times,
            is_left_associative: true,
        })),
        _ => panic!("Not yet implemented"),
    }
}

pub fn parse(tokens: &mut Vec<Token>, current_binding_power: i64) -> Result<Ast, ParseError> {
    // Pop the first token and find which parselet we should use
    let initial_token = tokens.pop().unwrap();
    let initial_parselet = match initial_map(&initial_token) {
        None => return Err(ParseError("Unexpected Token".to_string())),
        Some(v) => v,
    };

    let mut left_node = (*initial_parselet).parse(tokens, initial_token)?;

    loop {
        // If next token is empty then stop repeating
        let next_token = match tokens.last() {
            None => break,
            Some(v) => v,
        };

        // If we don't find a consequent parselet then stop repeating
        let consequent_parselet = match consequent_map(next_token) {
            None => break,
            Some(v) => v,
        };

        if get_binding_power(token_op_to_ast_op(next_token)) <= current_binding_power {
            break;
        };

        // Consume the token we peeped
        let next_token = tokens.pop().unwrap();

        left_node = consequent_parselet.parse(tokens, left_node, next_token)?;
    }

    return Ok(left_node);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_number() {
        let input: &mut Vec<Token> = &mut vec![Token::Number(1)];
        input.reverse();

        let result = parse(input, 0).unwrap();
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

        let result = parse(input, 0).unwrap();
        let expected_output = Ast::OperatorNode(
            Op::Plus,
            Box::new(Ast::NumberNode(1)),
            Box::new(Ast::OperatorNode(
                Op::Times,
                Box::new(Ast::NumberNode(2)),
                Box::new(Ast::NumberNode(3)),
            )),
        );
        assert_eq!(result, expected_output);
    }
}
