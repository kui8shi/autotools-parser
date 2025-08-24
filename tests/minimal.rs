#![deny(rust_2018_idioms)]
use autotools_parser::ast::minimal::AcCommand;
use autotools_parser::ast::minimal::Command;
use autotools_parser::ast::minimal::Command::*;
use autotools_parser::ast::minimal::CompoundCommand;
use autotools_parser::ast::minimal::Condition;
use autotools_parser::ast::minimal::Operator;
use autotools_parser::ast::minimal::Word;
use autotools_parser::ast::minimal::WordFragment;
use autotools_parser::ast::MayM4;
use autotools_parser::ast::MayM4::*;
use autotools_parser::ast::Redirect;
use autotools_parser::lexer::Lexer;
use autotools_parser::m4_macro::M4Argument;
use autotools_parser::m4_macro::SideEffect;
use autotools_parser::m4_macro::Var;
use autotools_parser::parse::autoconf::MinimalParser;
mod minimal_util;
use minimal_util::*;

pub fn make_parser(src: &str) -> MinimalParser<Lexer<std::str::Chars<'_>>> {
    MinimalParser::new_with_config(Lexer::new(src.chars()), true)
}

#[test]
fn test_macro_with_raw_literal() {
    let input = r#"AC_ARG_ENABLE([val], ,[case "${val}" in
        yes) flag=true ;;
        no)  flag=false ;;
        *)   AC_MSG_ERROR([bad value ${val}]) ;;
    esac])"#;

    let mut p = make_parser(input);
    dbg!(p.complete_command().unwrap().unwrap());
}

#[test]
fn test_macro_call_with_empty_parentheses() {
    let mut p = make_parser("AC_MSG_ERROR()");
    let correct = m4_macro_as_cmd("AC_MSG_ERROR", &[m4_lit("")]);
    assert_eq!(correct, p.complete_command().unwrap().unwrap());
}

#[test]
fn test_nested_macro_calls() {
    let input = r#"AC_MSG_CHECKING([for FEATURE in $EXAMPLE_DIR/sub/path])
AC_CACHE_VAL([example_cv_feature],
    [AS_IF([test -d $EXAMPLE_DIR/sub/path],
        [example_cv_feature=yes],
        [example_cv_feature=no])])
AC_MSG_RESULT([$example_cv_feature])"#;

    let p = make_parser(input);
    for res in p {
        match res {
            Ok(cmd) => {
                dbg!(cmd);
            }
            Err(e) => {
                println!("{}", e);
                panic!();
            }
        }
    }
}

#[test]
fn test_macro_with_simple_array_argument() {
    let input = r#"
dnl blank-separated array
AC_CONFIG_FILES([
Makefile
src/Makefile
src/codepages/Makefile
doc/Makefile
programs/Makefile
examples/Makefile
test/Makefile
test/unit-testing/Makefile
libredwg.pc
vcpkg.json
])


# Don't demand an m4 unless it's actually needed.
if test $found_asm = yes; then
  AC_ARG_VAR(M4,[m4 macro processor])
  AC_CACHE_CHECK([for suitable m4],
  gmp_cv_prog_m4,
  [if test -n "$M4"; then
    gmp_cv_prog_m4="$M4"
  else
    cat >conftest.m4 <<\EOF
  [define(dollarhash,``$][#'')ifelse(dollarhash(x),1,`define(t1,Y)',
  ``bad: $][# not supported (SunOS /usr/bin/m4)
  '')ifelse(eval(89),89,`define(t2,Y)',
  `bad: eval() doesnt support 8 or 9 in a constant (OpenBSD 2.6 m4)
  ')ifelse(eval(9,9),10,`define(t3,Y)',
  `bad: eval() doesnt support radix in eval (FreeBSD 8.x,9.0,9.1,9.2 m4)
  ')ifelse(t1`'t2`'t3,YYY,`good
  ')]
EOF
  echo "trying m4" >&5
  gmp_tmp_val=`(m4 conftest.m4) 2>&5`
  echo "$gmp_tmp_val" >&5
  if test "$gmp_tmp_val" = good; then
    gmp_cv_prog_m4="m4"
  else
    IFS="${IFS= 	}"; ac_save_ifs="$IFS"; IFS=":"
    ac_dummy="$PATH:/usr/5bin"
    for ac_dir in $ac_dummy; do
      test -z "$ac_dir" && ac_dir=.
      echo "trying $ac_dir/m4" >&5
      gmp_tmp_val=`($ac_dir/m4 conftest.m4) 2>&5`
      echo "$gmp_tmp_val" >&5
      if test "$gmp_tmp_val" = good; then
        gmp_cv_prog_m4="$ac_dir/m4"
        break
      fi
    done
    IFS="$ac_save_ifs"
    if test -z "$gmp_cv_prog_m4"; then
      AC_MSG_ERROR([No usable m4 in \$PATH or /usr/5bin (see config.log for reasons).])
    fi
  fi
  rm -f conftest.m4
fi])
M4="$gmp_cv_prog_m4"
AC_SUBST(M4)

  AC_CACHE_CHECK([if m4wrap produces spurious output],
               gmp_cv_m4_m4wrap_spurious,
[# hide the d-n-l from autoconf's error checking
tmp_d_n_l=d""nl
cat >conftest.m4 <<EOF
[changequote({,})define(x,)m4wrap({x})$tmp_d_n_l]
EOF
echo test input is >&5
cat conftest.m4 >&5
tmp_chars=`$M4 conftest.m4 | wc -c`
echo produces $tmp_chars chars output >&5
rm -f conftest.m4
if test $tmp_chars = 0; then
  gmp_cv_m4_m4wrap_spurious=no
else
  gmp_cv_m4_m4wrap_spurious=yes
fi
])
echo ["define(<M4WRAP_SPURIOUS>,<$gmp_cv_m4_m4wrap_spurious>)"] >> $gmp_tmpconfigm4


# else
# It's unclear why this m4-not-needed stuff was ever done.
#  if test -z "$M4" ; then
#    M4=m4-not-needed
#  fi
fi
dnl comma-separated array
AC_CHECK_TYPES([intmax_t, long double, long long, ptrdiff_t, quad_t,
		uint_least32_t, intptr_t])
"#;
    let p = make_parser(input);
    for res in p {
        match res {
            Ok(cmd) => {
                dbg!(cmd);
            }
            Err(e) => {
                println!("{}", e);
                panic!();
            }
        }
    }
}

#[test]
fn test_macro_with_special_characters() {
    let input = r#"AC_DEFINE([EXAMPLE_PATH], ["${prefix}/share/example\nx=`date`\n"], [Path with special characters])"#;

    let mut p = make_parser(input);
    let cmd = p.complete_command().unwrap();
    assert!(cmd.is_some());
    dbg!(&cmd);
}

#[test]
fn test_macro_with_complex_array_argument() {
    let input = r#"AC_CHECK_FUNCS([a
                                   b=arg
                                   c[with-brackets]
                                   "d with spaces"], 
                                  [ACTION-IF-FOUND], 
                                  [ACTION-IF-NOT-FOUND])"#;

    let mut p = make_parser(input);
    let cmd = p.complete_command().unwrap();
    assert!(cmd.is_some());
    dbg!(&cmd);
}

#[test]
fn test_macro_with_shell_command_containing_brackets() {
    let input = r#"AC_SUBST([EXAMPLE], [`find $srcdir -name "*.h" | grep -v test | sort | sed 's/\(.*\)/"\1" \\/g'`])"#;

    let mut p = make_parser(input);
    let cmd = p.complete_command().unwrap();
    assert!(cmd.is_some());
    dbg!(&cmd);
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
    let input = r#"test "$foo" = "yes" && [foo]=1"#;
    let mut p = make_parser(input);

    // Create expected structure with AND condition
    let expected = MinimalCommand::new_compound(CompoundCommand::And(
        Condition::Cond(Operator::Eq(word_var("foo"), word_lit("yes"))),
        Box::new(AcCommand::new_assign("foo".into(), word_lit("1"))),
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
        may_lit("WORD_"),
        m4_macro_as_word("MACRO", &[m4_raw("$var"), m4_raw("arg2"), m4_raw("arg3")]),
        may_lit("_SUFFIX"),
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
                cmd_from_words("echo", &[word_var("var")]),
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
            m4_word(Macro(as_help_string)),
            m4_cmd(assign("ENABLE_OPTION", word_var("enableval"))),
            m4_cmd(assign("ENABLE_OPTION", word_lit("default"))),
        ],
    );
    // supplement side effects of AC_ARG_ENABLE
    ac_arg_enable.effects = Some(SideEffect {
        shell_vars: Some(vec![
            "enableval".into(),
            "option".into(),
            Var::define_input("enable_option"),
        ]),
        ..Default::default()
    });
    let expected = cmd_macro(ac_arg_enable);
    let mut p = make_parser(input);
    assert_eq!(p.complete_command().unwrap().unwrap(), expected);
}

#[test]
fn test_unevaluated_define_macro() {
    let input = r#"echo ["define(<VAR>,<$var>)"] >> config.m4"#;

    let expected = AcCommand::new_compound(CompoundCommand::Redirect(
        Box::new(AcCommand::new_cmd(vec![
            word_lit("echo"),
            word(double_quoted(&[
                lit("define(<VAR>,<"),
                var("var"),
                lit(">)"),
            ])),
        ])),
        vec![Redirect::Append(None, word_lit("config.m4"))],
    ));

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
        cond(eq(word_var("ENABLE_OPTION"), word_lit("no"))),
        &[assign("var", word_var("configdir"))],
    );

    let mut p = make_parser(input);
    assert_eq!(p.complete_command().unwrap().unwrap(), expected);
}

#[test]
fn test_condition_concat_via_and() {
    let input = r#"
[if test "$foo" = "1" -a "$bar" = "0" ; then
  var="yes"
fi]"#;

    let expected = cmd_if(
        cond_and(cond(eq(word_var("foo"), word_lit("1"))), cond(eq(word_var("bar"), word_lit("0")))),
        &[assign("var", word_lit("yes"))],
    );

    let mut p = make_parser(input);
    assert_eq!(p.complete_command().unwrap().unwrap(), expected);
}

#[test]
fn test_condition_concat_via_or() {
    let input = r#"
[if test "$foo" = "1" -o "$bar" = "0" ; then
  var="yes"
fi]"#;

    let expected = cmd_if(
        cond_or(cond(eq(word_var("foo"), word_lit("1"))), cond(eq(word_var("bar"), word_lit("0")))),
        &[assign("var", word_lit("yes"))],
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
            word_var("enableval"),
            &[
                (
                    &[
                        word_lit("yes"),
                        word_lit("no"),
                        words(&[
                            Shell(WordFragment::SquareOpen),
                            may_lit("02468"),
                            Shell(WordFragment::SquareClose),
                        ]),
                        words(&[
                            Shell(WordFragment::SquareOpen),
                            may_lit("0-9"),
                            Shell(WordFragment::SquareClose),
                            Shell(WordFragment::SquareOpen),
                            may_lit("02468"),
                            Shell(WordFragment::SquareClose),
                        ]),
                    ],
                    &[],
                ),
                (
                    &[words(&[
                        Shell(WordFragment::Star),
                        Shell(WordFragment::SquareOpen),
                        may_lit("13579"),
                        Shell(WordFragment::SquareClose),
                    ])],
                    &[m4_macro_as_cmd(
                        "AC_MSG_ERROR",
                        &[m4_lit("bad value, only even sizes supported")],
                    )],
                ),
                (
                    &[word(Shell(WordFragment::Star))],
                    &[m4_macro_as_cmd(
                        "AC_MSG_ERROR",
                        &[m4_lit("bad value, need yes/no/number")],
                    )],
                ),
            ],
        )),
        m4_cmd(assign("enable_size", word_lit("no"))),
    ];

    let mut p = make_parser(input);
    match p.complete_command() {
        Ok(Some(cmd)) => {
            if let Compound(MayM4::Macro(m)) = *cmd.cmd {
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
    let echo_cmd = MinimalCommand::new_cmd(vec![word_lit("echo"), word_var("name")]);

    let sed_cmd = MinimalCommand::new_cmd(vec![
        word_lit("sed"),
        word_lit("-e"),
        word_lit(r"/\//s:^[^/]*/::"),
        word_lit("-e"),
        word_lit("s:[\\/]:_:g"),
    ]);

    // Create the pipeline command
    let piped_cmd =
        MinimalCommand::new_compound(CompoundCommand::Pipe(false, vec![echo_cmd, sed_cmd]));

    // Create the entire backticked expression
    let backtick_expr = Shell(WordFragment::Subst(Box::new(
        MinimalParameterSubstitution::Command(vec![piped_cmd]),
    )));

    // Create the assignment command
    let assign_expr = AcCommand::new_assign("var".into(), Word::Single(backtick_expr).into());

    // Create the expected define macro
    let expected = MinimalCommand::new_macro(m4_macro(
        "m4_esyscmd",
        &[M4Argument::Commands(vec![assign_expr])],
    ));

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
    let input = "AC_INIT(PROGRAM, [m4_bpatsubst(m4_bpatsubst(m4_esyscmd(
                grep \"^#define VERSION \" config.h.in /dev/null 2>/dev/null),
                [^.*VERSION\t+],
                []
              ),
              [\t*$],
              []
            ).m4_bpatsubst(m4_bpatsubst(m4_esyscmd(
                    grep \"^#define VERSION_MINOR \" config.h.in /dev/null 2>/dev/null),
                    [^.*VERSION_MINOR \t+],
                    []
                  ),
                  [\t*$],
                  []
                ).m4_bpatsubst(m4_bpatsubst(m4_esyscmd(
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
    let version_cmd = AcCommand::new_compound(CompoundCommand::Redirect(
        Box::new(AcCommand::new_cmd(vec![
            word_lit("grep"),
            word_lit("^#define VERSION "),
            word_lit("config.h.in"),
            word_lit("/dev/null"),
        ])),
        vec![Redirect::Write(Some(2), word_lit("/dev/null"))],
    ));

    let version_minor_cmd = AcCommand::new_compound(CompoundCommand::Redirect(
        Box::new(AcCommand::new_cmd(vec![
            word_lit("grep"),
            word_lit("^#define VERSION_MINOR "),
            word_lit("config.h.in"),
            word_lit("/dev/null"),
        ])),
        vec![Redirect::Write(Some(2), word_lit("/dev/null"))],
    ));

    let version_patchlevel_cmd = AcCommand::new_compound(CompoundCommand::Redirect(
        Box::new(AcCommand::new_cmd(vec![
            word_lit("grep"),
            word_lit("^#define VERSION_PATCHLEVEL "),
            word_lit("config.h.in"),
            word_lit("/dev/null"),
        ])),
        vec![Redirect::Write(Some(2), word_lit("/dev/null"))],
    ));

    // Create nested m4 macros for the version components
    let version_esyscmd = m4_macro("m4_esyscmd", &[M4Argument::Commands(vec![version_cmd])]);
    let version_patsubst1 = m4_macro(
        "m4_bpatsubst",
        &[
            M4Argument::Word(Word::Single(Macro(version_esyscmd)).into()),
            m4_lit("^.*VERSION\t+"),
            m4_lit(""),
        ],
    );
    let version_patsubst2 = m4_macro(
        "m4_bpatsubst",
        &[
            M4Argument::Word(Word::Single(Macro(version_patsubst1)).into()),
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
            M4Argument::Word(Word::Single(Macro(version_minor_esyscmd)).into()),
            m4_lit("^.*VERSION_MINOR \t+"),
            m4_lit(""),
        ],
    );
    let version_minor_patsubst2 = m4_macro(
        "m4_bpatsubst",
        &[
            M4Argument::Word(Word::Single(Macro(version_minor_patsubst1)).into()),
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
            M4Argument::Word(Word::Single(Macro(version_patchlevel_esyscmd)).into()),
            m4_lit("^.*VERSION_PATCHLEVEL\t+"),
            m4_lit(""),
        ],
    );
    let version_patchlevel_patsubst2 = m4_macro(
        "m4_bpatsubst",
        &[
            M4Argument::Word(Word::Single(Macro(version_patchlevel_patsubst1)).into()),
            m4_lit("*$"),
            m4_lit(""),
        ],
    );

    // Combine all components into the version string with dot separators
    let expected_patsubst_arg = Word::Concat(vec![
        Macro(version_patsubst2),
        may_lit("."),
        Macro(version_minor_patsubst2),
        may_lit("."),
        Macro(version_patchlevel_patsubst2),
    ])
    .into();

    match p.complete_command() {
        Ok(Some(cmd)) => {
            if let Command::Compound(MayM4::Macro(m)) = *cmd.cmd {
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
