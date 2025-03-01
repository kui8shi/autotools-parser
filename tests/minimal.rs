#![deny(rust_2018_idioms)]
use autoconf_parser::lexer::Lexer;
use autoconf_parser::parse::{MinimalParser, ParseErrorKind::*};

mod parse_support;

pub fn make_parser_minimal(src: &str) -> MinimalParser<Lexer<std::str::Chars<'_>>> {
    MinimalParser::new(Lexer::new(src.chars()))
}

#[test]
fn test_macro_minimal_unusual_style_of_newline() {
    let input = r#"dnl  GMP_VERSION
dnl  -----------
dnl  The gmp version number, extracted from the #defines in gmp-h.in at
dnl  autoconf time.  Two digits like 3.0 if patchlevel <= 0, or three digits
dnl  like 3.0.1 if patchlevel > 0.

define(GMP_VERSION,
[GMP_HEADER_GETVAL(__GNU_MP_VERSION,gmp-h.in)[]dnl
.GMP_HEADER_GETVAL(__GNU_MP_VERSION_MINOR,gmp-h.in)[]dnl
.GMP_HEADER_GETVAL(__GNU_MP_VERSION_PATCHLEVEL,gmp-h.in)])
"#;
    let mut p = make_parser_minimal(input);
    dbg!(p.complete_command().unwrap());
}

#[test]
fn test_macro_minimal_t() {
    let input =
        r#"GMP_DEFINE_RAW("define_not_for_expansion(\`HAVE_DOUBLE_IEEE_BIG_ENDIAN')", POST)"#;
    let mut p = make_parser_minimal(input);
    dbg!(p.complete_command().unwrap());
}

#[test]
fn test_minimal_condition() {
    let input = r#"test "$foo" = "yes" && foo=1"#;
    let mut p = make_parser_minimal(input);
    dbg!(p.complete_command().unwrap());
}

#[test]
fn test_macro_word_with_empty_quotes() {
    let input = r#"WORD_[]MACRO([$1],[arg2],[arg3])_SUFFIX)"#;
    let mut p = make_parser_minimal(input);
    match p.word() {
        Ok(c) => {
            dbg!(c);
        }
        Err(e) => println!("{}", e),
    }
}

#[test]
fn test_minimal_pipeline() {
    let input = r#"m4_if([$1],,,[echo hello; echo hi])"#;
    let mut p = make_parser_minimal(input);
    for i in 0..10 {
        match p.complete_command() {
            Ok(c) => {
                dbg!(c);
            }
            Err(e) => println!("{}", e),
        }
    }
}
