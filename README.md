# Skiff

A gradually-typed, functional scripting language with a friendly syntax and interpreter written in Rust!

## Running

You can run Skiff in the [Skiff web editor](https://skiff.paulbiberstein.me/) (powered by WASM!).

If you prefer to use your own text editor, you can install Skiff from [crates.io](https://crates.io/) and run it from the command line

```bash
cargo install skiff
skiff <filename> # make sure installed crate binaries are in your PATH
```

## About

Skiff started as a personal project for me to learn more about the design and implementation of programming languages. It was a mash-up of ideas and syntaxes from existing languages. As it evolved, however, it became a platform for me to learn about different algorithms like HM type inference and exhaustiveness checking of pattern match expressions.

Next on the road map is an exploration of gradual typing by adding a `typed` keyword to distinguish fully type functions from partially typed functions. By default, Skiff will have very few static guarantees. However, you can opt into more checks within a given function by fully annotating the arguments and return type or using the `typed` keyword to tell Skiff to infer a function type. The goal is to have a language that is as easy to use as a dynamically-typed language while offering some of the guarantees and in-code documentation of statically-typed languages.

## What does it look like?

```
# function definition (types are optional)
def fact(n: Number) -> Number:
    match n:
        | 1 => 1
        | n =>
            let next = fact(n - 1)
            next * n
    end
end
```

```
# conditionals
let cond: Boolean = true
if cond:
    1
elif false:
    2
else:
    3
end
```

```
# algebraic datatypes (types are optional)
data Option:
    | some(v: Number)
    | empty()
end
```

```
# pattern matching
match some(1):
    | some(n) => n
    | empty() => 0
end
```

```
# anonymous functions
let increment: Number -> Number = lambda(n): n + 1 end
let add: (Number, Number) -> Number = lambda(a,b): a + b end
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

## Features

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
| `typed` keyword          |                       |                      |
| Strings                  |                       |                      |
| File Operations          |                       |                      |
| Testing Constructs       |                       |                      |

Miscellaneous:

- [ ] REPL
- [ ] Language Reference
- [x] Web Editor (WASM)
- [x] Continuous Integration
- [x] Publish crate
