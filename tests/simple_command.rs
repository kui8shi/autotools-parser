#![deny(rust_2018_idioms)]
use autotools_parser::ast::minimal::*;
use autotools_parser::ast::Redirect::*;

mod minimal_util;
use crate::minimal_util::*;

#[test]
fn test_simple_command_valid_assignments_at_start_of_command() {
    let mut p = make_parser("var=val ENV=true BLANK= foo bar baz");
    let correct = cmd_brace(&[
        assign("var", word_lit("val")),
        assign("ENV", word_lit("true")),
        assign("BLANK", Word::Empty.into()),
        cmd_from_lits("foo", &["bar", "baz"]),
    ]);
    assert_eq!(correct, p.simple_command().unwrap());
}

#[test]
fn test_simple_command_assignments_after_start_of_command_should_be_args() {
    let mut p = make_parser("var=val ENV=true BLANK= foo var2=val2 bar baz var3=val3");
    let correct = cmd_brace(&[
        assign("var", word_lit("val")),
        assign("ENV", word_lit("true")),
        assign("BLANK", Word::Empty.into()),
        cmd_from_lits("foo", &["var2=val2", "bar", "baz", "var3=val3"]),
    ]);
    assert_eq!(correct, p.simple_command().unwrap());
}

#[test]
fn test_simple_command_redirections_at_end_of_command() {
    let mut p = make_parser("var=val ENV=true BLANK= foo bar baz 2>|clob 3<>rw <in");
    let correct = cmd_brace(&[
        assign("var", word_lit("val")),
        assign("ENV", word_lit("true")),
        assign("BLANK", Word::Empty.into()),
        redirect(
            cmd_from_lits("foo", &["bar", "baz"]),
            &[
                Clobber(Some(2), word_lit("clob")),
                ReadWrite(Some(3), word_lit("rw")),
                Read(None, word_lit("in")),
            ],
        ),
    ]);
    assert_eq!(correct, p.simple_command().unwrap());
}
