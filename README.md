# Skiff

An immutability-first, functional scripting language with a friendly syntax and interpreter written in Rust!

## Running

```bash
cargo run -- <filename>
```

For example:

```bash
cargo run -- tests/files/success/plus_and_times_precedence.boat
```

## Roadmap

Language Features:

|                          | Tree Walk Interpreter | Bytecode Interpreter |
| ------------------------ | --------------------- | -------------------- |
| Arithmetic               | &check;               |                      |
| Equality Operators       | &check;               |                      |
| Conditionals             | &check;               |                      |
| Functions                | &check;               |                      |
| Recursion                | &check;               |                      |
| Lambdas                  | &check;               |                      |
| Let binding              | &check;               |                      |
| Improved Error Reporting | &check;               |                      |
| Type Annotations         | &check;               |                      |
| Type Inference           | &check;               |                      |
| Algebraic Datatypes      | &check;               |                      |
| Pattern Matching         | &check;               |                      |
| Exhaustiveness Checking  | &check;               |                      |
| Call Stack Traces        | &check;               |                      |
| Strings                  |                       |                      |
| File Operations          |                       |                      |
| Testing Constructs       |                       |                      |

Miscellaneous:

- [ ] REPL
- [ ] Language Reference
- [x] Web Editor (WASM)
- [ ] Differential Tester
- [x] Continuous Integration
- [x] Publish crate
