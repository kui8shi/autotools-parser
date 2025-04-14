#![deny(rust_2018_idioms)]
use autoconf_parser::ast::minimal::*;
use autoconf_parser::ast::Redirect;
use autoconf_parser::ast::minimal::CompoundCommand;
use autoconf_parser::ast::minimal::Command::*;
use autoconf_parser::lexer::Lexer;
use autoconf_parser::m4_macro::M4Argument;
use autoconf_parser::m4_macro::SideEffect;
use autoconf_parser::m4_macro::Var;
use autoconf_parser::parse::MinimalParser;
mod minimal_util;
use minimal_util::*;

pub fn make_parser(src: &str) -> MinimalParser<Lexer<std::str::Chars<'_>>> {
    MinimalParser::new(Lexer::new(src.chars()))
}

#[test]
fn test_macro_with_unusual_style_of_newline() {
    let input = r#"dnl comment
MACRO(arg1, arg2)[]dnl unusual style of commentf
"#;
    let mut p = make_parser(input);
    let expected = m4_macro_as_cmd("MACRO", &[m4_raw("arg1"), m4_raw("arg2")]);
    let result = p.complete_command();
    match result {
        Ok(c) => assert_eq!(c, Some(expected)),
        Err(e) => {
            println!("{}", e);
            panic!();
        }
    }
}

#[test]
fn test_condition() {
    let input = r#"test "$foo" = "yes" && foo=1"#;
    let mut p = make_parser(input);

    // Create expected structure with AND condition
    let expected = MinimalCommand::new(Compound(
        CompoundCommand::And(
            Condition::Cond(Operator::Eq(word(var("foo")), word(lit("yes")))),
            Box::new(CommandWrapper::new(Assignment("foo".into(), word(lit("1"))))),
        ),
    ));

    match p.complete_command() {
        Ok(cmd) => {
            assert_eq!(cmd, Some(expected));
        }
        Err(e) => {
            println!("{}", e);
            panic!();
        }
    }
}

#[test]
fn test_macro_word_and_empty_quotes() {
    let input = r#"WORD_[]MACRO([$var],[arg2],[arg3])[]_SUFFIX)"#;
    let mut p = make_parser(input);
    let expected = words(&[
        lit("WORD_"),
        m4_macro_as_word("MACRO", &[m4_raw("$var"), m4_raw("arg2"), m4_raw("arg3")]),
        lit("_SUFFIX"),
    ]);
    match p.word() {
        Ok(w) => assert_eq!(w, Some(expected)),
        Err(e) => {
            println!("{}", e);
            panic!();
        }
    }
}

#[test]
fn test_minimal_macro_with_quoted_command_group() {
    let input = r#"m4_if([$var],,[echo found; echo $var],[])"#;
    let mut p = make_parser(input);
    let expected = m4_macro_as_cmd(
        "m4_if",
        &[
            m4_var("var"),
            m4_lit(""),
            m4_cmds(&[
                cmd_from_lits("echo", &["found"]),
                cmd_from_words("echo", &[word(var("var"))]),
            ]),
            m4_cmds(&[]),
        ],
    );
    match p.complete_command() {
        Ok(c) => assert_eq!(c, Some(expected)),
        Err(e) => {
            println!("{}", e);
            panic!();
        }
    }
}

#[test]
fn test_unquoted_arguments_and_linebreaks() {
    let input = r#"
AC_ARG_ENABLE(option,
AS_HELP_STRING([--disable-option],
[if set, do not use option]),
ENABLE_OPTION=$enableval,
ENABLE_OPTION=default)"#;
    // construct
    let as_help_string = m4_macro(
        "AS_HELP_STRING",
        &[
            m4_lit("--disable-option"),
            m4_lit("if set, do not use option"),
        ],
    );
    let mut ac_arg_enable = m4_macro(
        "AC_ARG_ENABLE",
        &[
            m4_lit("option"),
            m4_word(WordFragment::Macro(as_help_string)),
            m4_cmd(assign("ENABLE_OPTION", word(var("enableval")))),
            m4_cmd(assign("ENABLE_OPTION", word(lit("default")))),
        ],
    );
    // supplement side effects of AC_ARG_ENABLE
    ac_arg_enable.effects = Some(SideEffect {
        shell_vars: Some(vec!["option".into(), Var::new_input("enable_option")]),
        ..Default::default()
    });
    let expected = cmd_macro(ac_arg_enable);
    let mut p = make_parser(input);
    assert_eq!(p.complete_command().unwrap().unwrap(), expected);
}

#[test]
fn test_quoted_command_in_root() {
    let input = r#"
[if test "${ENABLE_OPTION}" = "no" ; then
  var="$configdir"
fi]"#;

    let expected = cmd_if(
        cond(eq(word(var("ENABLE_OPTION")), word(lit("no")))),
        &[assign("var", word(var("configdir")))],
    );

    let mut p = make_parser(input);
    assert_eq!(p.complete_command().unwrap().unwrap(), expected);
}

#[test]
fn test_quoted_patterns_in_macro_argument() {
    let input = r#"
AC_ARG_ENABLE(size,
AS_HELP_STRING(--enable-size,[use size specified [default=no]]),
[case $enableval in
[yes|no|[02468]|[0-9][02468]]) ;;
[*[13579]])
  AC_MSG_ERROR([bad value, only even sizes supported]) ;;
*)
  AC_MSG_ERROR([bad value, need yes/no/number]) ;;
esac],
[enable_size=no])
"#;
    let expected_args = vec![
        m4_lit("size"),
        m4_word(m4_macro_as_word(
            "AS_HELP_STRING",
            &[
                m4_lit("--enable-size"),
                m4_lit("use size specified [default=no]"),
            ],
        )),
        m4_cmd(cmd_case(
            word(var("enableval")),
            &[
                (
                    &[
                        word(lit("yes")),
                        word(lit("no")),
                        words(&[
                            WordFragment::SquareOpen,
                            lit("02468"),
                            WordFragment::SquareClose,
                        ]),
                        words(&[
                            WordFragment::SquareOpen,
                            lit("0-9"),
                            WordFragment::SquareClose,
                            WordFragment::SquareOpen,
                            lit("02468"),
                            WordFragment::SquareClose,
                        ]),
                    ],
                    &[],
                ),
                (
                    &[words(&[
                        WordFragment::Star,
                        WordFragment::SquareOpen,
                        lit("13579"),
                        WordFragment::SquareClose,
                    ])],
                    &[m4_macro_as_cmd(
                        "AC_MSG_ERROR",
                        &[m4_lit("bad value, only even sizes supported")],
                    )],
                ),
                (
                    &[word(WordFragment::Star)],
                    &[m4_macro_as_cmd(
                        "AC_MSG_ERROR",
                        &[m4_lit("bad value, need yes/no/number")],
                    )],
                ),
            ],
        )),
        m4_cmd(assign("enable_size", word(lit("no")))),
    ];

    let mut p = make_parser(input);
    match p.complete_command() {
        Ok(Some(cmd)) => {
            if let Compound(CompoundCommand::Macro(m)) = cmd.cmd {
                assert_eq!("AC_ARG_ENABLE", m.name);
                for (actual, expected) in m.args.iter().zip(expected_args.iter()) {
                    assert_eq!(actual, expected);
                }
            } else {
                println!("{:?}", cmd);
                panic!();
            }
        }
        Ok(None) => {
            panic!();
        }
        Err(e) => {
            println!("{}", e);
            panic!();
        }
    }
}

#[test]
fn test_backticked_command_in_macro_argument() {
    let input = r#"m4_esyscmd([var=`echo $name | sed -e '/\//s:^[^/]*/::' -e 's:[\\/]:_:g'`])"#;
    let mut p = make_parser(input);

    // Create the backticked shell command for the sed expression
    let echo_cmd = MinimalCommand::new(Cmd(vec![word(lit("echo")), word(var("name"))]));

    let sed_cmd = MinimalCommand::new(Cmd(
        vec![
            word(lit("sed")),
            word(lit("-e")),
            word(lit(r"/\//s:^[^/]*/::")),
            word(lit("-e")),
            word(lit("s:[\\/]:_:g")),
        ],
    ));

    // Create the pipeline command
    let piped_cmd =
        MinimalCommand::new(Compound(CompoundCommand::Pipe(false, vec![echo_cmd, sed_cmd])));

    // Create the entire backticked expression
    let backtick_expr = WordFragment::Subst(Box::new(MinimalParameterSubstitution::Command(vec![
        piped_cmd,
    ])));

    // Create the assignment command
    let assign_expr = CommandWrapper::new(Assignment("var".into(), Word::Single(backtick_expr)));

    // Create the expected define macro
    let expected = MinimalCommand::new(Compound(
        CompoundCommand::Macro(m4_macro(
            "m4_esyscmd",
            &[M4Argument::Commands(vec![assign_expr])],
        )),
    )
    );

    match p.complete_command() {
        Ok(cmd) => {
            assert_eq!(Some(expected), cmd);
        }
        Err(e) => {
            println!("{}", e);
            panic!();
        }
    }
}

#[test]
fn test_macro_patsubst_nested() {
    let input = "AC_INIT(PROGRAM, [patsubst(patsubst(esyscmd(
                grep \"^#define VERSION \" config.h.in /dev/null 2>/dev/null),
                [^.*VERSION\t+],
                []
              ),
              [\t*$],
              []
            ).patsubst(patsubst(esyscmd(
                    grep \"^#define VERSION_MINOR \" config.h.in /dev/null 2>/dev/null),
                    [^.*VERSION_MINOR \t+],
                    []
                  ),
                  [\t*$],
                  []
                ).patsubst(patsubst(esyscmd(
                        grep \"^#define VERSION_PATCHLEVEL \" config.h.in /dev/null 2>/dev/null),
                        [^.*VERSION_PATCHLEVEL\t+],
                        []
                      ),
                      [\t*$],
                      []
                    )],
          PROGRAM.tar
    )";
    let mut p = make_parser(input);

    // Create expected structures for version commands
    let version_cmd = MinimalCommand::new(Compound(
        CompoundCommand::Redirect(
            Box::new(CommandWrapper::new(Cmd(
                vec![
                    word(lit("grep")),
                    word(lit("^#define VERSION ")),
                    word(lit("config.h.in")),
                    word(lit("/dev/null")),
                ],
            ))),
            Redirect::Write(Some(2), word(lit("/dev/null"))),
        ),
    ));

    let version_minor_cmd = CommandWrapper::new(Compound(
        CompoundCommand::Redirect(
            Box::new(CommandWrapper::new(Cmd(
                vec![
                    word(lit("grep")),
                    word(lit("^#define VERSION_MINOR ")),
                    word(lit("config.h.in")),
                    word(lit("/dev/null")),
                ],
            ))),
            Redirect::Write(Some(2), word(lit("/dev/null"))),
        ),
    ));

    let version_patchlevel_cmd = CommandWrapper::new(Compound(
        CompoundCommand::Redirect(
            Box::new(CommandWrapper::new(Cmd(
                vec![
                    word(lit("grep")),
                    word(lit("^#define VERSION_PATCHLEVEL ")),
                    word(lit("config.h.in")),
                    word(lit("/dev/null")),
                ],
            ))),
            Redirect::Write(Some(2), word(lit("/dev/null"))),
        ),
    ));

    // Create nested m4 macros for the version components
    let version_esyscmd = m4_macro("m4_esyscmd", &[M4Argument::Commands(vec![version_cmd])]);
    let version_patsubst1 = m4_macro(
        "m4_bpatsubst",
        &[
            M4Argument::Word(Word::Single(WordFragment::Macro(version_esyscmd))),
            m4_lit("^.*VERSION\t+"),
            m4_lit(""),
        ],
    );
    let version_patsubst2 = m4_macro(
        "m4_bpatsubst",
        &[
            M4Argument::Word(Word::Single(WordFragment::Macro(version_patsubst1))),
            m4_lit("*$"),
            m4_lit(""),
        ],
    );

    // Version minor component
    let version_minor_esyscmd = m4_macro(
        "m4_esyscmd",
        &[M4Argument::Commands(vec![version_minor_cmd])],
    );
    let version_minor_patsubst1 = m4_macro(
        "m4_bpatsubst",
        &[
            M4Argument::Word(Word::Single(WordFragment::Macro(version_minor_esyscmd))),
            m4_lit("^.*VERSION_MINOR \t+"),
            m4_lit(""),
        ],
    );
    let version_minor_patsubst2 = m4_macro(
        "m4_bpatsubst",
        &[
            M4Argument::Word(Word::Single(WordFragment::Macro(version_minor_patsubst1))),
            m4_lit("*$"),
            m4_lit(""),
        ],
    );

    // Version patchlevel component
    let version_patchlevel_esyscmd = m4_macro(
        "m4_esyscmd",
        &[M4Argument::Commands(vec![version_patchlevel_cmd])],
    );
    let version_patchlevel_patsubst1 = m4_macro(
        "m4_bpatsubst",
        &[
            M4Argument::Word(Word::Single(WordFragment::Macro(
                version_patchlevel_esyscmd,
            ))),
            m4_lit("^.*VERSION_PATCHLEVEL\t+"),
            m4_lit(""),
        ],
    );
    let version_patchlevel_patsubst2 = m4_macro(
        "m4_bpatsubst",
        &[
            M4Argument::Word(Word::Single(WordFragment::Macro(
                version_patchlevel_patsubst1,
            ))),
            m4_lit("*$"),
            m4_lit(""),
        ],
    );

    // Combine all components into the version string with dot separators
    let expected_patsubst_arg = Word::Concat(vec![
        WordFragment::Macro(version_patsubst2),
        lit("."),
        WordFragment::Macro(version_minor_patsubst2),
        lit("."),
        WordFragment::Macro(version_patchlevel_patsubst2),
    ]);

    match p.complete_command() {
        Ok(Some(cmd)) => {
            if let Compound(CompoundCommand::Macro(m)) = cmd.cmd {
                assert_eq!("AC_INIT", m.name);
                assert_eq!(
                    vec![
                        m4_lit("PROGRAM"),
                        M4Argument::Word(expected_patsubst_arg),
                        m4_lit("PROGRAM.tar")
                    ],
                    m.args
                );
            } else {
                println!("{:?}", cmd);
                panic!();
            }
        }
        Ok(None) => {
            panic!();
        }
        Err(e) => {
            println!("{}", e);
            panic!();
        }
    }
}
