use logos::{Lexer, Logos};

fn string_token(lex: &mut Lexer<Token>) -> Option<String> {
    let slice = lex.slice();
    Some(slice[1..slice.len() - 1].into())
}
#[derive(Logos)]
pub enum Token {
    #[token("{")]
    LBracket,
    #[token("}")]
    RBracket,
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
    #[token("[0-9]+", |lex| lex.slice().parse())]
    Number(i64),

    #[token("[a-zA-Z][a-zA-z0-9]*", |lex| lex.slice().parse())]
    Identifier(String),

    #[token(r#""[^"]*""#, string_token)]
    String(String),
    #[error]
    #[regex(r"[ \t\n\f]+", logos::skip)]
    Error,
}
