#![deny(rust_2018_idioms)]
use autotools_parser::ast::minimal::AcCommand;
use autotools_parser::ast::minimal::Command;
use autotools_parser::ast::minimal::CompoundCommand;
use autotools_parser::ast::MayM4;
use autotools_parser::token::Token;

mod minimal_util;
use crate::minimal_util::*;

#[test]
fn test_complete_command_non_eager_parse() {
    let mut p = make_parser("foo && bar; baz\n\nqux");
    let cmd1 = p
        .complete_command()
        .unwrap()
        .expect("failed to parse first command");
    let cmd2 = p
        .complete_command()
        .unwrap()
        .expect("failed to parse second command");
    let cmd3 = p
        .complete_command()
        .unwrap()
        .expect("failed to parse third command");

    let correct1 = cmd_and(cond_return_zero(cmd("foo")), cmd("bar"));
    let correct2 = cmd("baz");
    let correct3 = cmd("qux");

    assert_eq!(correct1, cmd1);
    assert_eq!(correct2, cmd2);
    assert_eq!(correct3, cmd3);
}

#[test]
fn test_complete_command_valid_no_input() {
    let mut p = make_parser("");
    p.complete_command().expect("no input caused an error");
}

#[test]
fn test_command_delegates_valid_commands_brace() {
    let correct = cmd_brace(&[cmd("foo")]);
    assert_eq!(correct, make_parser("{ foo; }").command().unwrap());
}

#[test]
fn test_command_delegates_valid_commands_subshell() {
    let commands = ["(foo)", "( foo)"];

    let correct = cmd_subshell(cmd("foo"));

    for cmd in &commands {
        match make_parser(cmd).command() {
            Ok(ref result) if result == &correct => {}
            Ok(result) => panic!(
                "Parsed \"{}\" as an unexpected command type:\n{:#?}",
                cmd, result
            ),
            Err(err) => panic!("Failed to parse \"{}\": {}", cmd, err),
        }
    }
}

#[test]
fn test_command_delegates_valid_commands_while() {
    let correct = cmd_while(cond_return_zero(cmd("guard")), &[cmd("foo")]);
    assert_eq!(
        correct,
        make_parser("while guard; do foo; done").command().unwrap()
    );
}

#[test]
fn test_command_delegates_valid_commands_until() {
    let correct = cmd_until(cond_return_zero(cmd("guard")), &[cmd("foo")]);
    assert_eq!(
        correct,
        make_parser("until guard; do foo; done").command().unwrap()
    );
}

#[test]
fn test_command_delegates_valid_commands_for() {
    let correct = cmd_for("var", &[], &[cmd("foo")]);
    assert_eq!(
        correct,
        make_parser("for var in; do foo; done").command().unwrap()
    );
}

#[test]
fn test_command_delegates_valid_commands_if() {
    let correct = cmd_if(cond_return_zero(cmd("guard")), &[cmd("body")]);
    assert_eq!(
        correct,
        make_parser("if guard; then body; fi").command().unwrap()
    );
}

#[test]
fn test_command_delegates_valid_commands_case() {
    let correct = cmd_case(word_lit("foo"), &[]);
    assert_eq!(correct, make_parser("case foo in esac").command().unwrap());
}

#[test]
fn test_command_delegates_valid_simple_commands() {
    let correct = cmd_from_lits("echo", &["foo", "bar"]);
    assert_eq!(correct, make_parser("echo foo bar").command().unwrap());
}

#[test]
fn test_command_delegates_valid_commands_function() {
    // @kui8shi
    // Disallow non whitespaces before parentheses without funciton keyword
    let commands = [
        "function foo()      { echo body; }",
        "function foo ()     { echo body; }",
        "function foo (    ) { echo body; }",
        "function foo(    )  { echo body; }",
        "function foo        { echo body; }",
        // "foo()               { echo body; }",
        "foo ()              { echo body; }",
        "foo (    )          { echo body; }",
        // "foo(    )           { echo body; }",
    ];

    let correct = cmd_from_compound(CompoundCommand::FunctionDef {
        name: "foo".into(),
        body: Box::new(cmd_brace(&[cmd_from_lits("echo", &["body"])])),
    });
    for cmd in &commands {
        match make_parser(cmd).command() {
            Ok(ref result) if result == &correct => {}
            Ok(result) => panic!(
                "Parsed \"{}\" as an unexpected command type:\n{:#?}",
                cmd, result
            ),
            Err(err) => panic!("Failed to parse \"{}\": {}", cmd, err),
        }
    }
}

#[test]
fn test_command_parses_quoted_compound_commands_as_simple_commands() {
    let cases = [
        "{foo; }", // { is a reserved word, thus concatenating it essentially "quotes" it
        "'{' foo; }",
        "'(' foo; )",
        "'while' guard do foo; done",
        "'until' guard do foo; done",
        "'if' guard; then body; fi",
        "'for' var in; do echo $var; done",
        "'function' name { echo body; }",
        "name'()' { echo body; }",
        "123fn() { echo body; }",
        "'case' foo in esac",
        "\"{\" foo; }",
        "\"(\" foo; )",
        "\"while\" guard do foo; done",
        "\"until\" guard do foo; done",
        "\"if\" guard; then body; fi",
        "\"for\" var in; do echo $var; done",
        "\"function\" name { echo body; }",
        "name\"()\" { echo body; }",
        "\"case\" foo in esac",
    ];

    for cmd in &cases {
        match make_parser(cmd).command() {
            Ok(cmd) => {
                if let Command::Cmd(_) = *cmd.cmd {
                } else {
                    panic!(
                        "Parse::command unexpectedly parsed \"{:?}\" as a non-simple command:\n",
                        cmd
                    )
                }
            }
            Err(err) => panic!("Parse::command failed to parse \"{}\": {}", cmd, err),
        }
    }
}

#[test]
fn test_command_should_delegate_literals_and_names_loop_while() {
    for kw in [
        Token::Literal(String::from("while")),
        Token::Name(String::from("while")),
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

        let cmd = p.command().unwrap();
        if let Command::Compound(ref compound_cmd) = *cmd.cmd {
            if let MayM4::Shell(CompoundCommand::While(_)) = compound_cmd {
                continue;
            }
        }

        panic!("Parsed an unexpected command:\n{:#?}", cmd)
    }
}

#[test]
fn test_command_should_delegate_literals_and_names_loop_until() {
    for kw in [
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

        let cmd = p.command().unwrap();
        if let Command::Compound(ref compound_cmd) = *cmd.cmd {
            if let MayM4::Shell(CompoundCommand::Until(_)) = compound_cmd {
                continue;
            }
        }

        panic!("Parsed an unexpected command:\n{:#?}", cmd)
    }
}

#[test]
fn test_command_should_delegate_literals_and_names_if() {
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

                        let cmd = p.command().unwrap();
                        if let Command::Compound(ref compound_cmd) = *cmd.cmd {
                            if let MayM4::Shell(CompoundCommand::If {
                                conditionals: _,
                                else_branch: _,
                            }) = compound_cmd
                            {
                                continue;
                            }
                        }

                        panic!("Parsed an unexpected command:\n{:#?}", cmd)
                    }
                }
            }
        }
    }
}

#[test]
fn test_command_should_delegate_literals_and_names_for() {
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

            let cmd = p.command().unwrap();
            if let Command::Compound(ref compound_cmd) = *cmd.cmd {
                if let MayM4::Shell(CompoundCommand::For {
                    var: _,
                    words: _,
                    body: _,
                }) = compound_cmd
                {
                    continue;
                }
            }
            panic!("Parsed an unexpected command:\n{:#?}", cmd)
        }
    }
}

#[test]
fn test_command_should_delegate_literals_and_names_case() {
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

                let cmd = p.command().unwrap();
                if let Command::Compound(ref compound_cmd) = *cmd.cmd {
                    if let MayM4::Shell(CompoundCommand::Case { word: _, arms: _ }) = compound_cmd {
                        continue;
                    }
                }

                panic!("Parsed an unexpected command:\n{:#?}", cmd)
            }
        }
    }
}

#[test]
fn test_command_should_delegate_literals_and_names_for_function_declaration() {
    for fn_tok in [
        Token::Literal(String::from("function")),
        Token::Name(String::from("function")),
    ] {
        let mut p = make_parser_from_tokens(vec![
            fn_tok,
            Token::Whitespace(String::from(" ")),
            Token::Name(String::from("fn_name")),
            Token::Whitespace(String::from(" ")),
            Token::ParenOpen,
            Token::ParenClose,
            Token::Whitespace(String::from(" ")),
            Token::CurlyOpen,
            Token::Whitespace(String::from(" ")),
            Token::Literal(String::from("echo")),
            Token::Whitespace(String::from(" ")),
            Token::Literal(String::from("fn body")),
            Token::Semi,
            Token::CurlyClose,
        ]);
        match p.command() {
            Ok(cmd) => {
                if let Command::Compound(MayM4::Shell(CompoundCommand::FunctionDef {
                    name: _,
                    body: _,
                })) = *cmd.cmd
                {
                } else {
                    panic!("Parsed an unexpected command type:\n{:#?}", cmd);
                }
            }
            Err(err) => panic!("Failed to parse command: {}", err),
        }
    }
}

#[test]
fn test_command_do_not_delegate_functions_only_if_fn_name_is_a_literal_token() {
    let mut p = make_parser_from_tokens(vec![
        Token::Literal(String::from("fn_name")),
        Token::Whitespace(String::from(" ")),
        Token::ParenOpen,
        Token::ParenClose,
        Token::Whitespace(String::from(" ")),
        Token::CurlyOpen,
        Token::Literal(String::from("echo")),
        Token::Whitespace(String::from(" ")),
        Token::Literal(String::from("fn body")),
        Token::Semi,
        Token::CurlyClose,
    ]);
    match p.command() {
        Ok(cmd) => {
            if let Command::Cmd(_) = *cmd.cmd {
            } else {
                panic!("Parsed an unexpected command type:\n{:#?}", cmd);
            }
        }
        Err(err) => panic!("Failed to parse command: {}", err),
    }
}

#[test]
fn test_command_delegate_functions_only_if_fn_name_is_a_name_token() {
    let mut p = make_parser_from_tokens(vec![
        Token::Name(String::from("fn_name")),
        Token::Whitespace(String::from(" ")),
        Token::ParenOpen,
        Token::ParenClose,
        Token::Whitespace(String::from(" ")),
        Token::CurlyOpen,
        Token::Whitespace(String::from(" ")),
        Token::Literal(String::from("echo")),
        Token::Whitespace(String::from(" ")),
        Token::Literal(String::from("fn body")),
        Token::Semi,
        Token::CurlyClose,
    ]);
    match p.command() {
        Ok(cmd) => {
            if let Command::Compound(MayM4::Shell(CompoundCommand::FunctionDef {
                name: _,
                body: _,
            })) = *cmd.cmd
            {
            } else {
                panic!("Parsed an unexpected command type:\n{:#?}", cmd);
            }
        }
        Err(err) => panic!("Failed to parse command: {}", err),
    }
}
