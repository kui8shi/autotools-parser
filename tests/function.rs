#![deny(rust_2018_idioms)]
use autotools_parser::ast::minimal::CompoundCommand::*;
use autotools_parser::parse::ParseErrorKind::*;
use autotools_parser::token::Token;

mod minimal_util;
use minimal_util::*;

#[test]
fn test_function_declaration_valid() {
    let correct = cmd_from_compound(FunctionDef {
        name: String::from("foo"),
        body: Box::new(cmd_brace(&[cmd_from_lits("echo", &["body"])])),
    });

    assert_eq!(
        correct,
        make_parser("function foo()      { echo body; }")
            .function_declaration()
            .unwrap()
    );
    assert_eq!(
        correct,
        make_parser("function foo ()     { echo body; }")
            .function_declaration()
            .unwrap()
    );
    assert_eq!(
        correct,
        make_parser("function foo (    ) { echo body; }")
            .function_declaration()
            .unwrap()
    );
    assert_eq!(
        correct,
        make_parser("function foo(    )  { echo body; }")
            .function_declaration()
            .unwrap()
    );
    assert_eq!(
        correct,
        make_parser("function foo        { echo body; }")
            .function_declaration()
            .unwrap()
    );
    assert_eq!(
        correct,
        make_parser("foo()               { echo body; }")
            .function_declaration()
            .unwrap()
    );
    assert_eq!(
        correct,
        make_parser("foo ()              { echo body; }")
            .function_declaration()
            .unwrap()
    );
    assert_eq!(
        correct,
        make_parser("foo (    )          { echo body; }")
            .function_declaration()
            .unwrap()
    );
    assert_eq!(
        correct,
        make_parser("foo(    )           { echo body; }")
            .function_declaration()
            .unwrap()
    );

    assert_eq!(
        correct,
        make_parser("function foo()     \n{ echo body; }")
            .function_declaration()
            .unwrap()
    );
    assert_eq!(
        correct,
        make_parser("function foo ()    \n{ echo body; }")
            .function_declaration()
            .unwrap()
    );
    assert_eq!(
        correct,
        make_parser("function foo (    )\n{ echo body; }")
            .function_declaration()
            .unwrap()
    );
    assert_eq!(
        correct,
        make_parser("function foo(    ) \n{ echo body; }")
            .function_declaration()
            .unwrap()
    );
    assert_eq!(
        correct,
        make_parser("function foo       \n{ echo body; }")
            .function_declaration()
            .unwrap()
    );
    assert_eq!(
        correct,
        make_parser("foo()              \n{ echo body; }")
            .function_declaration()
            .unwrap()
    );
    assert_eq!(
        correct,
        make_parser("foo ()             \n{ echo body; }")
            .function_declaration()
            .unwrap()
    );
    assert_eq!(
        correct,
        make_parser("foo (    )         \n{ echo body; }")
            .function_declaration()
            .unwrap()
    );
    assert_eq!(
        correct,
        make_parser("foo(    )          \n{ echo body; }")
            .function_declaration()
            .unwrap()
    );
}

#[test]
fn test_function_declaration_valid_body_need_not_be_a_compound_command() {
    let src = vec![
        ("function foo()      echo body;", src(20, 1, 21)),
        ("function foo ()     echo body;", src(20, 1, 21)),
        ("function foo (    ) echo body;", src(20, 1, 21)),
        ("function foo(    )  echo body;", src(20, 1, 21)),
        ("function foo        echo body;", src(20, 1, 21)),
        ("foo()               echo body;", src(20, 1, 21)),
        ("foo ()              echo body;", src(20, 1, 21)),
        ("foo (    )          echo body;", src(20, 1, 21)),
        ("foo(    )           echo body;", src(20, 1, 21)),
        ("function foo()     \necho body;", src(20, 2, 1)),
        ("function foo ()    \necho body;", src(20, 2, 1)),
        ("function foo (    )\necho body;", src(20, 2, 1)),
        ("function foo(    ) \necho body;", src(20, 2, 1)),
        ("function foo       \necho body;", src(20, 2, 1)),
        ("foo()              \necho body;", src(20, 2, 1)),
        ("foo ()             \necho body;", src(20, 2, 1)),
        ("foo (    )         \necho body;", src(20, 2, 1)),
        ("foo(    )          \necho body;", src(20, 2, 1)),
    ];

    for (s, p) in src {
        let correct = Unexpected(Token::Name(String::from("echo")), p).into();
        match make_parser(s).function_declaration() {
            Ok(w) => panic!("Unexpectedly parsed the source \"{}\" as\n{:?}", s, w),
            Err(ref err) => {
                if err != &correct {
                    panic!(
                        "Expected the source \"{}\" to return the error `{:?}`, but got `{:?}`",
                        s, correct, err
                    );
                }
            }
        }
    }
}

#[test]
fn test_function_declaration_parens_can_be_subshell_if_function_keyword_present() {
    let correct = cmd_from_compound(FunctionDef {
        name: String::from("foo"),
        body: Box::new(cmd_subshell(cmd_from_lits("echo", &["subshell"]))),
    });

    assert_eq!(
        correct,
        make_parser("function foo (echo subshell)")
            .function_declaration()
            .unwrap()
    );
    assert_eq!(
        correct,
        make_parser("function foo() (echo subshell)")
            .function_declaration()
            .unwrap()
    );
    assert_eq!(
        correct,
        make_parser("function foo () (echo subshell)")
            .function_declaration()
            .unwrap()
    );
    assert_eq!(
        correct,
        make_parser("function foo\n(echo subshell)")
            .function_declaration()
            .unwrap()
    );
}

#[test]
fn test_function_declaration_invalid_newline_in_declaration() {
    let mut p = make_parser("function\nname() { echo body; }");
    assert_eq!(
        Err(Unexpected(Token::Newline, src(8, 1, 9)).into()),
        p.function_declaration()
    );
    // If the function keyword is present the () are optional, and at this particular point
    // they become an empty subshell (which is invalid)
    let mut p = make_parser("function name\n() { echo body; }");
    assert_eq!(
        Err(Unexpected(Token::ParenClose, src(15, 2, 2)).into()),
        p.function_declaration()
    );
}

#[test]
fn test_function_declaration_invalid_missing_space_after_fn_keyword_and_no_parens() {
    let mut p = make_parser("functionname { echo body; }");
    assert_eq!(
        Err(Unexpected(Token::CurlyOpen, src(13, 1, 14)).into()),
        p.function_declaration()
    );
}

#[test]
fn test_function_declaration_invalid_missing_fn_keyword_and_parens() {
    let mut p = make_parser("name { echo body; }");
    assert_eq!(
        Err(Unexpected(Token::CurlyOpen, src(5, 1, 6)).into()),
        p.function_declaration()
    );
}

#[test]
fn test_function_declaration_invalid_missing_space_after_name_no_parens() {
    let mut p = make_parser("function name{ echo body; }");
    assert_eq!(
        Err(Unexpected(Token::CurlyOpen, src(13, 1, 14)).into()),
        p.function_declaration()
    );
    let mut p = make_parser("function name( echo body; )");
    assert_eq!(
        Err(Unexpected(Token::Name(String::from("echo")), src(15, 1, 16)).into()),
        p.function_declaration()
    );
}

#[test]
fn test_function_declaration_invalid_missing_name() {
    let mut p = make_parser("function { echo body; }");
    assert_eq!(
        Err(Unexpected(Token::CurlyOpen, src(9, 1, 10)).into()),
        p.function_declaration()
    );
    let mut p = make_parser("function () { echo body; }");
    assert_eq!(
        Err(Unexpected(Token::ParenOpen, src(9, 1, 10)).into()),
        p.function_declaration()
    );
    let mut p = make_parser("() { echo body; }");
    assert_eq!(
        Err(Unexpected(Token::ParenOpen, src(0, 1, 1)).into()),
        p.function_declaration()
    );
}

#[test]
fn test_function_declaration_invalid_missing_body() {
    let mut p = make_parser("function name");
    assert_eq!(Err(UnexpectedEOF.into()), p.function_declaration());
    let mut p = make_parser("function name()");
    assert_eq!(Err(UnexpectedEOF.into()), p.function_declaration());
    let mut p = make_parser("name()");
    assert_eq!(Err(UnexpectedEOF.into()), p.function_declaration());
}

#[test]
fn test_function_declaration_invalid_quoted() {
    let cmds = [
        (
            "'function' name { echo body; }",
            Unexpected(Token::SingleQuote, src(0, 1, 1)).into(),
        ),
        (
            "function 'name'() { echo body; }",
            Unexpected(Token::SingleQuote, src(9, 1, 10)).into(),
        ),
        (
            "name'()' { echo body; }",
            Unexpected(Token::SingleQuote, src(4, 1, 5)).into(),
        ),
        (
            "\"function\" name { echo body; }",
            Unexpected(Token::DoubleQuote, src(0, 1, 1)).into(),
        ),
        (
            "function \"name\"() { echo body; }",
            Unexpected(Token::DoubleQuote, src(9, 1, 10)).into(),
        ),
        (
            "name\"()\" { echo body; }",
            Unexpected(Token::DoubleQuote, src(4, 1, 5)).into(),
        ),
    ];

    for (c, e) in &cmds {
        match make_parser(c).function_declaration() {
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
fn test_function_declaration_invalid_fn_must_be_name() {
    let mut p = make_parser("function 123fn { echo body; }");
    assert_eq!(
        Err(BadIdent(String::from("123fn"), src(9, 1, 10)).into()),
        p.function_declaration()
    );
    let mut p = make_parser("function 123fn() { echo body; }");
    assert_eq!(
        Err(BadIdent(String::from("123fn"), src(9, 1, 10)).into()),
        p.function_declaration()
    );
    let mut p = make_parser("123fn() { echo body; }");
    assert_eq!(
        Err(BadIdent(String::from("123fn"), src(0, 1, 1)).into()),
        p.function_declaration()
    );
}

#[test]
fn test_function_declaration_invalid_fn_name_must_be_name_token() {
    let mut p = make_parser_from_tokens(vec![
        Token::Literal(String::from("function")),
        Token::Whitespace(String::from(" ")),
        Token::Literal(String::from("fn_name")),
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
    assert_eq!(
        Err(BadIdent(String::from("fn_name"), src(9, 1, 10)).into()),
        p.function_declaration()
    );

    let mut p = make_parser_from_tokens(vec![
        Token::Literal(String::from("function")),
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
    p.function_declaration().unwrap();
}

#[test]
fn test_function_declaration_invalid_concat() {
    let mut p = make_parser_from_tokens(vec![
        Token::Literal(String::from("func")),
        Token::Literal(String::from("tion")),
        Token::Whitespace(String::from(" ")),
        Token::Name(String::from("fn_name")),
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
    assert_eq!(
        Err(BadIdent(String::from("func"), src(0, 1, 1)).into()),
        p.function_declaration()
    );
}

#[test]
fn test_function_declaration_should_recognize_literals_and_names_for_fn_keyword() {
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
        p.function_declaration().unwrap();
    }
}
