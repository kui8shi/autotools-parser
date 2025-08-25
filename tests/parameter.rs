#![deny(rust_2018_idioms)]
use autotools_parser::ast::Parameter::*;
use autotools_parser::ast::ParameterSubstitution::*;
use autotools_parser::parse::ParseErrorKind::*;

mod minimal_util;
use crate::minimal_util::*;

#[test]
fn test_parameter_short() {
    let words = vec![At, Star, Pound, Question, Dash, Dollar, Bang, Positional(3)];

    let mut parser = make_parser("$@$*$#$?$-$$$!$3$");
    for p in words {
        assert_eq!(parser.parameter().unwrap(), word(param(p)));
    }

    assert_eq!(word_lit("$"), parser.parameter().unwrap());
    assert_eq!(Err(UnexpectedEOF.into()), parser.parameter()); // Stream should be exhausted
}

#[test]
fn test_parameter_short_in_curlies() {
    let words = vec![
        At,
        Star,
        Pound,
        Question,
        Dash,
        Dollar,
        Bang,
        Var(String::from("foo")),
        Positional(3),
        Positional(1000),
    ];

    let mut p = make_parser("${@}${*}${#}${?}${-}${$}${!}${foo}${3}${1000}");
    for pa in words {
        assert_eq!(p.parameter().unwrap(), word(param(pa)));
    }

    assert_eq!(Err(UnexpectedEOF.into()), p.parameter()); // Stream should be exhausted
}

#[test]
fn test_parameter_command_substitution() {
    let correct = word(subst(Command(vec![
        cmd_from_lits("echo", &["hello"]),
        cmd_from_lits("echo", &["world"]),
    ])));

    assert_eq!(
        correct,
        make_parser("$(echo hello; echo world)")
            .parameter()
            .unwrap()
    );
}

#[test]
fn test_parameter_command_substitution_valid_empty_substitution() {
    let correct = word(subst(Command(vec![])));
    assert_eq!(correct, make_parser("$()").parameter().unwrap());
    assert_eq!(correct, make_parser("$(     )").parameter().unwrap());
    assert_eq!(correct, make_parser("$(\n\n)").parameter().unwrap());
}

#[test]
fn test_parameter_literal_dollar_if_no_param() {
    let mut p = make_parser("$%asdf");
    assert_eq!(word_lit("$"), p.parameter().unwrap());
    assert_eq!(p.word().unwrap().unwrap(), word_lit("%asdf"));
}
