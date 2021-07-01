use logos::{Lexer, Logos};

fn string_token(lex: &mut Lexer<Token>) -> Option<String> {
    let slice = lex.slice();
    Some(slice[1..slice.len() - 1].into())
}
#[derive(Logos, Debug, Clone, PartialEq, Hash)]
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
    #[token(",")]
    Comma,
    #[token(":")]
    Colon,
    #[token("|")]
    Pipe,
    #[token("=>")]
    FatArrow,
    #[token("->")]
    ThinArrow,
    #[token("end")]
    End,
    #[token("data")]
    Data,
    #[token("match")]
    Match,
    #[token("let")]
    Let,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("elif")]
    Elif,
    #[token("lambda")]
    Lambda,
    #[token("def")]
    Def,
    #[token("-")]
    Minus,
    #[token("+")]
    Plus,
    #[token("*")]
    Times,
    #[token("/")]
    Divide,
    #[token("**")]
    Exp,
    #[token("=")]
    Eq,
    #[token("==")]
    DoubleEq,
    #[token("%")]
    Modulo,
    #[token(">")]
    Gt,
    #[token("<")]
    Lt,
    #[token(">=")]
    GtEq,
    #[token("<=")]
    LtEq,
    #[token("and")]
    LAnd,
    #[token("or")]
    LOr,
    #[token("&")]
    BitAnd,
    #[token("^")]
    BitXor,
    #[regex("[0-9]+", |lex| lex.slice().parse())]
    Number(i64),
    #[regex("[a-zA-Z][a-zA-z0-9]*", |lex| lex.slice().parse())]
    Identifier(String),
    #[token("true", |_| true)]
    #[token("false", |_| false)]
    Bool(bool),

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

    #[test]
    fn lexes_identifiers() {
        let mut lex = Token::lexer("x");

        assert_eq!(lex.next(), Some(Token::Identifier("x".to_string())));
        assert_eq!(lex.slice(), "x");
    }
}
