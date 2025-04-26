# autoconf-parser

A Rust library for parsing, analyzing, and splitting Autoconf scripts (`configure.ac` and similar files).

## Quick Start

First, add this to your `Cargo.toml`:

```toml
[dependencies]
autoconf-parser = "0.1.1"
```

Then you can get started with:

```rust
use autoconf_parser::lexer::Lexer;
use autoconf_parser::parse::MinimalParser;

fn main() {
    let input = r#"
AC_INIT([MyProject], [1.0])
AC_CONFIG_SRCDIR([src/main.c])
AC_OUTPUT
"#;

    // Initialize our token lexer and shell parser with the program's input
    let lex = Lexer::new(input.chars());
    let parser = MinimalParser::new(lex);

    // Parse our input!
    for t in parser {
        println!("{:?}", t);
    }
}
```

## About

`autoconf-parser` is designed for static analysis of Autoconf scripts,  
which are written in a combination of shell and M4 macros.

This library provides:
- Parsing of shell-like constructs (`if`, `case`, `for`, functions, etc.)
- Basic M4 macro parsing and expansion support
- An intermediate Abstract Syntax Tree (AST) representation
- Tools for analyzing and transforming build scripts
- Facilities for splitting and restructuring large configure scripts

It is intended for use cases such as:
- Migrating legacy Autoconf-based projects to modern build systems (e.g., Cargo)
- Static analysis or transformation of complex `configure.ac` scripts
- Research into build system structure

## Design Goals

- Provide a parser that can accurately handle typical `configure.ac` scripts
- Allow static analysis without executing or evaluating any script code
- Remain extensible toward richer M4 processing

## Non-goals

- Full compliance with the POSIX.1-2008 shell specification
- Emulating full runtime behavior of Autoconf scripts
- Feature parity with all major shell implementations

## Supported Grammar Features

- [x] Shell conditionals (`if`, `case`, `for`, `while`, `until`)
- [x] Pipelines and conditional execution (`&&`, `||`)
- [x] Compound commands (`{ }`, subshells `(...)`)
- [x] Function definitions
- [x] I/O redirections
- [x] Heredocs
- [x] Parameter expansions (`$var`, `${var}`)
- [x] Quoting (single, double, backtick, escape sequences)
- [x] Basic arithmetic expressions
- [x] M4 macro calls (partial)

## License

Licensed under either of:

- [MIT License](LICENSE-MIT) ([https://opensource.org/licenses/MIT](https://opensource.org/licenses/MIT))
- [Apache License, Version 2.0](LICENSE-APACHE) ([https://www.apache.org/licenses/LICENSE-2.0](https://www.apache.org/licenses/LICENSE-2.0))

at your option.

## Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in this project  
shall be dual licensed as above, without any additional terms or conditions.

---

Originally forked from [`conch-parser`](https://github.com/udoprog/conch-parser) by Ivan Petkov.
