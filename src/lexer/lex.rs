use logos::{Lexer, Logos};

fn string_token(lex: &mut Lexer<Token>) -> Option<String> {
    let slice = lex.slice();
    Some(slice[1..slice.len() - 1].into())
}
#[derive(Logos, Debug, PartialEq, Hash)]
pub enum Token {
    #[error]
    #[regex(r"[ \t\n\f]+", logos::skip)]
    Error,

    #[token("{")]
    LBracket,
    #[token("}")]
    RBracket,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token(".")]
    Dot,
    #[token(":")]
    Colon,
    #[token("end")]
    End,
    #[token("if")]
    If,
    #[token("-")]
    Minus,
    #[token("+")]
    Plus,
    #[token("*")]
    Times,
    #[regex("[0-9]+", |lex| lex.slice().parse())]
    Number(i64),
    #[token("[a-zA-Z][a-zA-z0-9]*", |lex| lex.slice().parse())]
    Identifier(String),

    #[token(r#""[^"]*""#, string_token)]
    String(String),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lexes_number() {
        let mut lex = Token::lexer("1");

        assert_eq!(lex.next(), Some(Token::Number(1)));
    }

    #[test]
    fn lexes_numbers_and_ops() {
        let mut lex = Token::lexer("1 + 2 * 3");

        assert_eq!(lex.next(), Some(Token::Number(1)));
        assert_eq!(lex.slice(), "1");

        assert_eq!(lex.next(), Some(Token::Plus));
        assert_eq!(lex.slice(), "+");

        assert_eq!(lex.next(), Some(Token::Number(2)));
        assert_eq!(lex.slice(), "2");

        assert_eq!(lex.next(), Some(Token::Times));
        assert_eq!(lex.slice(), "*");

        assert_eq!(lex.next(), Some(Token::Number(3)));
        assert_eq!(lex.slice(), "3");
    }
}
