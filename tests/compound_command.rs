#![deny(rust_2018_idioms)]
use autotools_parser::ast::condition::Condition;
use autotools_parser::ast::minimal::CompoundCommand;
use autotools_parser::ast::MayM4::*;
use autotools_parser::ast::{builder::*, Redirect};
use autotools_parser::parse::ParseErrorKind::*;
use autotools_parser::token::Token;

mod minimal_util;
use crate::minimal_util::*;

#[test]
fn test_do_group_valid() {
    let mut p = make_parser("do foo\nbar; baz\n#comment\n done");
    let correct = CommandGroup {
        commands: vec![cmd("foo"), cmd("bar"), cmd("baz")],
        trailing_comments: vec![Newline(Some("#comment".into()))],
    };
    assert_eq!(correct, p.do_group().unwrap());
}

#[test]
fn test_do_group_invalid_missing_separator() {
    let mut p = make_parser("do foo\nbar; baz done");
    assert_eq!(
        Err(IncompleteCmd("do", src(0, 1, 1), "done", src(20, 2, 14)).into()),
        p.do_group()
    );
}

#[test]
fn test_do_group_valid_keyword_delimited_by_separator() {
    let mut p = make_parser("do foo done; done");
    let correct = CommandGroup {
        commands: vec![cmd_from_lits("foo", &["done"])],
        trailing_comments: vec![],
    };
    assert_eq!(correct, p.do_group().unwrap());
}

#[test]
fn test_do_group_invalid_missing_keyword() {
    let mut p = make_parser("foo\nbar; baz; done");
    assert_eq!(
        Err(Unexpected(Token::Name(String::from("foo")), src(0, 1, 1)).into()),
        p.do_group()
    );
    let mut p = make_parser("do foo\nbar; baz");
    assert_eq!(
        Err(IncompleteCmd("do", src(0, 1, 1), "done", src(15, 2, 9)).into()),
        p.do_group()
    );
}

#[test]
fn test_do_group_invalid_quoted() {
    let cmds = [
        (
            "'do' foo\nbar; baz; done",
            Unexpected(Token::SingleQuote, src(0, 1, 1)).into(),
        ),
        (
            "do foo\nbar; baz; 'done'",
            IncompleteCmd("do", src(0, 1, 1), "done", src(23, 2, 17)).into(),
        ),
        (
            "\"do\" foo\nbar; baz; done",
            Unexpected(Token::DoubleQuote, src(0, 1, 1)).into(),
        ),
        (
            "do foo\nbar; baz; \"done\"",
            IncompleteCmd("do", src(0, 1, 1), "done", src(23, 2, 17)).into(),
        ),
    ];

    for (c, e) in &cmds {
        match make_parser(c).do_group() {
            Ok(result) => panic!("Unexpectedly parsed \"{}\" as\n{:#?}", c, result),
            Err(ref err) => {
                if err != e {
                    panic!(
                        "Expected the source \"{}\" to return the error `{:?}`, but got `{:?}`",
                        c, e, err
                    );
                }
            }
        }
    }
}

#[test]
fn test_do_group_invalid_concat() {
    let mut p = make_parser_from_tokens(vec![
        Token::Literal(String::from("d")),
        Token::Literal(String::from("o")),
        Token::Newline,
        Token::Literal(String::from("foo")),
        Token::Newline,
        Token::Literal(String::from("done")),
    ]);
    assert_eq!(
        Err(Unexpected(Token::Literal(String::from("d")), src(0, 1, 1)).into()),
        p.do_group()
    );
    let mut p = make_parser_from_tokens(vec![
        Token::Literal(String::from("do")),
        Token::Newline,
        Token::Literal(String::from("foo")),
        Token::Newline,
        Token::Literal(String::from("do")),
        Token::Literal(String::from("ne")),
    ]);
    assert_eq!(
        Err(IncompleteCmd("do", src(0, 1, 1), "done", src(11, 3, 5)).into()),
        p.do_group()
    );
}

#[test]
fn test_do_group_should_recognize_literals_and_names() {
    for do_tok in [
        Token::Literal(String::from("do")),
        Token::Name(String::from("do")),
    ] {
        for done_tok in [
            Token::Literal(String::from("done")),
            Token::Name(String::from("done")),
        ] {
            let mut p = make_parser_from_tokens(vec![
                do_tok.clone(),
                Token::Newline,
                Token::Literal(String::from("foo")),
                Token::Newline,
                done_tok,
            ]);
            p.do_group().unwrap();
        }
    }
}

#[test]
fn test_do_group_invalid_missing_body() {
    let mut p = make_parser("do\ndone");
    assert_eq!(
        Err(Unexpected(Token::Name("done".into()), src(3, 2, 1)).into()),
        p.do_group()
    );
}

#[test]
fn test_compound_command_delegates_valid_commands_brace() {
    let correct = cmd_brace(&[cmd("foo")]);
    assert_eq!(
        correct,
        make_parser("{ foo; }").complete_command().unwrap().unwrap()
    );
}

#[test]
fn test_compound_command_delegates_valid_commands_subshell() {
    let commands = ["(foo)", "( foo)", " (foo)", "\t(foo)", "\\\n(foo)"];

    let correct = cmd_subshell(cmd("foo"));

    for cmd in &commands {
        match make_parser(cmd).complete_command() {
            Ok(Some(ref result)) if result == &correct => {}
            Ok(result) => panic!(
                "Parsed \"{}\" as an unexpected command type:\n{:#?}",
                cmd, result
            ),
            Err(err) => panic!("Failed to parse \"{}\": {}", cmd, err),
        }
    }
}

#[test]
fn test_compound_command_delegates_valid_commands_while() {
    let correct = cmd_while(Condition::ReturnZero(Box::new(cmd("guard"))), &[cmd("foo")]);
    assert_eq!(
        correct,
        make_parser("while guard; do foo; done")
            .complete_command()
            .unwrap()
            .unwrap()
    );
}

#[test]
fn test_compound_command_delegates_valid_commands_until() {
    let correct = cmd_until(Condition::ReturnZero(Box::new(cmd("guard"))), &[cmd("foo")]);
    assert_eq!(
        correct,
        make_parser("until guard; do foo; done")
            .complete_command()
            .unwrap()
            .unwrap()
    );
}

#[test]
fn test_compound_command_delegates_valid_commands_for() {
    let correct = cmd_for("var", &[], &[cmd("foo")]);
    assert_eq!(
        correct,
        make_parser("for var in; do foo; done")
            .complete_command()
            .unwrap()
            .unwrap()
    );
}

#[test]
fn test_compound_command_delegates_valid_commands_if() {
    let correct = cmd_if(
        Condition::ReturnZero(Box::new(cmd("guard"))),
        &[cmd("body")],
    );
    assert_eq!(
        correct,
        make_parser("if guard; then body; fi")
            .complete_command()
            .unwrap()
            .unwrap()
    );
}

#[test]
fn test_compound_command_delegates_valid_commands_case() {
    let correct = cmd_case(word_lit("foo"), &[]);
    assert_eq!(
        correct,
        make_parser("case foo in esac")
            .complete_command()
            .unwrap()
            .unwrap()
    );
}

#[test]
fn test_compound_command_errors_on_quoted_commands() {
    let cases = [
        // { is a reserved word, thus concatenating it essentially "quotes" it
        // `compound_command` doesn't know or care enough to specify that "foo" is
        // the problematic token instead of {, however, callers who are smart enough
        // to expect a brace command would be aware themselves that no such valid
        // command actually exists. TL;DR: it's okay for `compound_command` to blame {
        ("{foo; }", Unexpected(Token::CurlyOpen, src(0, 1, 1)).into()),
        (
            "'{' foo; }",
            Unexpected(Token::SingleQuote, src(0, 1, 1)).into(),
        ),
        (
            "'(' foo; )",
            Unexpected(Token::SingleQuote, src(0, 1, 1)).into(),
        ),
        (
            "'while' guard do foo; done",
            Unexpected(Token::SingleQuote, src(0, 1, 1)).into(),
        ),
        (
            "'until' guard do foo; done",
            Unexpected(Token::SingleQuote, src(0, 1, 1)).into(),
        ),
        (
            "'if' guard; then body; fi",
            Unexpected(Token::SingleQuote, src(0, 1, 1)).into(),
        ),
        (
            "'for' var in; do foo; done",
            Unexpected(Token::SingleQuote, src(0, 1, 1)).into(),
        ),
        (
            "'case' foo in esac",
            Unexpected(Token::SingleQuote, src(0, 1, 1)).into(),
        ),
        (
            "\"{\" foo; }",
            Unexpected(Token::DoubleQuote, src(0, 1, 1)).into(),
        ),
        (
            "\"(\" foo; )",
            Unexpected(Token::DoubleQuote, src(0, 1, 1)).into(),
        ),
        (
            "\"while\" guard do foo; done",
            Unexpected(Token::DoubleQuote, src(0, 1, 1)).into(),
        ),
        (
            "\"until\" guard do foo; done",
            Unexpected(Token::DoubleQuote, src(0, 1, 1)).into(),
        ),
        (
            "\"if\" guard; then body; fi",
            Unexpected(Token::DoubleQuote, src(0, 1, 1)).into(),
        ),
        (
            "\"for\" var in; do foo; done",
            Unexpected(Token::DoubleQuote, src(0, 1, 1)).into(),
        ),
        (
            "\"case\" foo in esac",
            Unexpected(Token::DoubleQuote, src(0, 1, 1)).into(),
        ),
    ];

    for &(src, ref e) in &cases {
        match make_parser(src).compound_command() {
            Ok(result) => panic!(
                "Parse::compound_command unexpectedly succeeded parsing \"{}\" with result:\n{:#?}",
                src, result
            ),
            Err(ref err) => {
                if err != e {
                    panic!(
                        "Expected the source \"{}\" to return the error `{:?}`, but got `{:?}`",
                        src, e, err
                    );
                }
            }
        }
    }
}

#[test]
fn test_compound_command_captures_redirections_after_command() {
    let cases = [
        "{ foo; } 1>>out <& 2 2>&-",
        "( foo; ) 1>>out <& 2 2>&-",
        "while guard; do foo; done 1>>out <& 2 2>&-",
        "until guard; do foo; done 1>>out <& 2 2>&-",
        "if guard; then body; fi 1>>out <& 2 2>&-",
        "for var in; do foo; done 1>>out <& 2 2>&-",
        "case foo in esac 1>>out <& 2 2>&-",
    ];

    for cmd in &cases {
        match make_parser(cmd).compound_command() {
            Ok(Shell(CompoundCommand::Redirect(_, io))) => assert_eq!(
                vec![
                    Redirect::Append(Some(1), word_lit("out")),
                    Redirect::DupRead(None, word_lit("2")),
                    Redirect::DupWrite(Some(2), word_lit("-")),
                ],
                io
            ),
            Err(err) => panic!("Failed to parse \"{}\": {}", cmd, err),
            _ => panic!("Failed to parse \"{}\"", cmd),
        }
    }
}

#[test]
fn test_compound_command_should_delegate_literals_and_names_loop() {
    for kw in [
        Token::Literal(String::from("while")),
        Token::Name(String::from("while")),
        Token::Literal(String::from("until")),
        Token::Name(String::from("until")),
    ] {
        let mut p = make_parser_from_tokens(vec![
            kw,
            Token::Newline,
            Token::Literal(String::from("guard")),
            Token::Newline,
            Token::Literal(String::from("do")),
            Token::Newline,
            Token::Literal(String::from("foo")),
            Token::Newline,
            Token::Literal(String::from("done")),
        ]);
        p.compound_command().unwrap();
    }
}

#[test]
fn test_compound_command_should_delegate_literals_and_names_if() {
    for if_tok in [
        Token::Literal(String::from("if")),
        Token::Name(String::from("if")),
    ] {
        for then_tok in [
            Token::Literal(String::from("then")),
            Token::Name(String::from("then")),
        ] {
            for elif_tok in [
                Token::Literal(String::from("elif")),
                Token::Name(String::from("elif")),
            ] {
                for else_tok in [
                    Token::Literal(String::from("else")),
                    Token::Name(String::from("else")),
                ] {
                    for fi_tok in [
                        Token::Literal(String::from("fi")),
                        Token::Name(String::from("fi")),
                    ] {
                        let mut p = make_parser_from_tokens(vec![
                            if_tok.clone(),
                            Token::Whitespace(String::from(" ")),
                            Token::Literal(String::from("guard1")),
                            Token::Newline,
                            then_tok.clone(),
                            Token::Newline,
                            Token::Literal(String::from("body1")),
                            elif_tok.clone(),
                            Token::Whitespace(String::from(" ")),
                            Token::Literal(String::from("guard2")),
                            Token::Newline,
                            then_tok.clone(),
                            Token::Whitespace(String::from(" ")),
                            Token::Literal(String::from("body2")),
                            else_tok.clone(),
                            Token::Whitespace(String::from(" ")),
                            Token::Whitespace(String::from(" ")),
                            Token::Literal(String::from("else part")),
                            Token::Newline,
                            fi_tok,
                        ]);
                        p.compound_command().unwrap();
                    }
                }
            }
        }
    }
}

#[test]
fn test_compound_command_should_delegate_literals_and_names_for() {
    for for_tok in [
        Token::Literal(String::from("for")),
        Token::Name(String::from("for")),
    ] {
        for in_tok in [
            Token::Literal(String::from("in")),
            Token::Name(String::from("in")),
        ] {
            let mut p = make_parser_from_tokens(vec![
                for_tok.clone(),
                Token::Whitespace(String::from(" ")),
                Token::Name(String::from("var")),
                Token::Whitespace(String::from(" ")),
                in_tok.clone(),
                Token::Whitespace(String::from(" ")),
                Token::Literal(String::from("one")),
                Token::Whitespace(String::from(" ")),
                Token::Literal(String::from("two")),
                Token::Whitespace(String::from(" ")),
                Token::Literal(String::from("three")),
                Token::Whitespace(String::from(" ")),
                Token::Newline,
                Token::Literal(String::from("do")),
                Token::Whitespace(String::from(" ")),
                Token::Literal(String::from("echo")),
                Token::Whitespace(String::from(" ")),
                Token::Dollar,
                Token::Name(String::from("var")),
                Token::Newline,
                Token::Literal(String::from("done")),
            ]);
            p.compound_command().unwrap();
        }
    }
}

#[test]
fn test_compound_command_should_delegate_literals_and_names_case() {
    let case_str = String::from("case");
    let in_str = String::from("in");
    let esac_str = String::from("esac");
    for case_tok in [Token::Literal(case_str.clone()), Token::Name(case_str)] {
        for in_tok in [Token::Literal(in_str.clone()), Token::Name(in_str.clone())] {
            for esac_tok in [
                Token::Literal(esac_str.clone()),
                Token::Name(esac_str.clone()),
            ] {
                let mut p = make_parser_from_tokens(vec![
                    case_tok.clone(),
                    Token::Whitespace(String::from(" ")),
                    Token::Literal(String::from("foo")),
                    Token::Literal(String::from("bar")),
                    Token::Whitespace(String::from(" ")),
                    in_tok.clone(),
                    Token::Whitespace(String::from(" ")),
                    Token::Literal(String::from("foo")),
                    Token::ParenClose,
                    Token::Newline,
                    Token::Literal(String::from("echo")),
                    Token::Whitespace(String::from(" ")),
                    Token::Literal(String::from("foo")),
                    Token::Newline,
                    Token::Newline,
                    Token::DSemi,
                    esac_tok,
                ]);
                p.compound_command().unwrap();
            }
        }
    }
}
