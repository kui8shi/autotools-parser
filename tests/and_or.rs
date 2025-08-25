#![deny(rust_2018_idioms)]
use autotools_parser::parse::ParseErrorKind::*;
use autotools_parser::token::Token;

mod minimal_util;
use crate::minimal_util::*;

#[test]
fn test_and_or_correct_associativity() {
    let mut p = make_parser("foo || bar && baz");
    let correct = cmd_and(
        cond_or(cond_return_zero(cmd("foo")), cond_return_zero(cmd("bar"))),
        cmd("baz"),
    );
    assert_eq!(correct, p.and_or_list().unwrap());
}

#[test]
fn test_and_or_valid_with_newlines_after_operator() {
    let correct = cmd_and(
        cond_or(cond_return_zero(cmd("foo")), cond_return_zero(cmd("bar"))),
        cmd("baz"),
    );
    let mut p = make_parser("foo ||\n\n\n\nbar && baz");
    assert_eq!(correct, p.and_or_list().unwrap());
}

#[test]
fn test_and_or_invalid_with_newlines_before_operator() {
    let mut p = make_parser("foo || bar\n\n&& baz");
    p.and_or_list().unwrap(); // Successful parse Or(foo, bar)
                              // Fail to parse "&& baz" which is an error
    assert_eq!(
        Err(Unexpected(Token::AndIf, src(12, 3, 1)).into()),
        p.complete_command()
    );
}
