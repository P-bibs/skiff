# Skiff

An immutability-first, functional scripting language with a friendly syntax and interpreter written in Rust!

## Running

```bash
cargo run <filename>
```

For example:

```bash
cargo run test/plus_and_times_precedence.skf
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
| Type Annotations         |                       |                      |
| Algebraic Datatypes      | &check;               |                      |
| Pattern Matching         | &check;               |                      |
| Call Stack Traces        | &check;               |                      |
| Strings                  |                       |                      |
| File Operations          |                       |                      |
| Testing Constructs       |                       |                      |

Miscellaneous:

- [ ] REPL
- [ ] Language Reference
- [ ] Web Editor (WASM)
- [ ] Differential Tester
- [ ] Continuous Integration
- [ ] Publish crate
