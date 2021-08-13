# Skiff

An immutability-first, functional scripting language with a friendly syntax and interpreter written in Rust!

## Running

You can run Skiff in the [Skiff web editor](https://skiff.paulbiberstein.me/).

If you prefer to use your own text editor, you can install Skiff from crates.io and run it from the command line

```bash
cargo install skiff
skiff <filename> # make sure installed crate binaries are in your PATH
```

## Language Reference

Full docs are a work in progress. To get an idea of what the features and syntax look like, you can look at the [language tour test file](https://github.com/P-bibs/skiff/blob/master/tests/files/success/language_tour.boat).

## Developing

Clone the repo to work on Skiff. You can run a local development version using

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
| Parameterized Types      |                       |                      |
| Strings                  |                       |                      |
| File Operations          |                       |                      |
| Testing Constructs       |                       |                      |

Miscellaneous:

- [ ] REPL
- [ ] Language Reference
- [x] Web Editor (WASM)
- [x] Continuous Integration
- [x] Publish crate
