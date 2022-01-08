#[macro_use]
extern crate lazy_static;

pub mod ast;
pub mod error_handling;
pub mod runtime;
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
    pub mod util;
}

pub mod interpreter {
    pub mod interpret;
}

pub mod bytecode {
    pub mod compiler {
        pub mod bytecode_compile;
    }
    pub mod virtual_machine {
        pub mod virtual_machine;
    }
    pub mod common;
}

pub mod wasm {
    mod utils;
    pub mod wasm_exports;
}

pub mod static_checking {
    pub mod exhaustiveness;
}
