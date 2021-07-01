#[macro_use]
extern crate lazy_static;

pub mod ast;
pub mod error_handling;
pub mod lexer {
    pub mod lex;
}
pub mod parser {
    pub mod parse;
    pub mod parselets;
    pub mod util;
    mod patterns {
        pub mod parse;
        pub mod parselets;
    }
    mod types {
        pub mod parse;
    }
}

pub mod type_inferencer {
    pub mod ast;
    pub mod constraint_gen;
    pub mod type_inference;
    pub mod unification;
}

pub mod interpreter {
    pub mod interpret;
}

pub mod wasm {
    mod utils;
    pub mod wasm_exports;
}
