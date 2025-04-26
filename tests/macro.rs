#![deny(rust_2018_idioms)]
use autoconf_parser::ast::Command::*;
use autoconf_parser::ast::PipeableCommand::*;
use autoconf_parser::ast::*;
use autoconf_parser::m4_macro::M4Argument::*;
use autoconf_parser::m4_macro::M4Macro;

mod parse_support;
use crate::parse_support::*;

pub fn macro_as_command(name: &str, args: &[&str]) -> TopLevelCommand<String> {
    TopLevelCommand(List(CommandList {
        first: ListableCommand::Single(Compound(Box::new(CompoundCommand {
            kind: CompoundCommandKind::Macro(M4Macro::new(
                name.to_string(),
                args.iter().map(|a| Unknown(a.to_string())).collect(),
            )),
            io: vec![],
        }))),
        rest: vec![],
    }))
}

#[test]
fn test_macro_with_command() {
    let input = r#"AH_CONFIG_COMMANDS_PRE([echo hi])"#;
    let mut p = make_parser(input);
    let name = "AH_CONFIG_COMMANDS_PRE";
    let correct = M4Macro::new(
        name.to_string(),
        vec![Commands(vec![cmd_args("echo", &["hi"])])],
    );
    assert_eq!(correct, p.macro_call(&[name]).unwrap());
}

#[test]
fn test_macro_complex() {
    let input = r#"
AC_COMPILE_IFELSE([
    AC_LANG_SOURCE([example_feature int v;]) ], [
    AC_DEFINE([FEATURE_TYPE], [example_feature], [Feature specifier]) ], [
AC_COMPILE_IFELSE([
    AC_LANG_SOURCE([__example int v;]) ], [
    AC_DEFINE([FEATURE_TYPE], [__example], [Feature specifier]) ], [
AC_COMPILE_IFELSE([
    AC_LANG_SOURCE([__attribute((example)) int v;]) ], [
    AC_DEFINE([FEATURE_TYPE], [__attribute((example))], [Feature specifier]) ], [
])])])
"#;

    let p = make_parser(input);
    for res in p {
        match res {
            Ok(cmd) => println!("{:?}", cmd),
            Err(e) => {
                println!("{}", e);
                panic!();
            }
        }
    }
}

#[test]
fn test_macro_with_trailing_comments() {
    let input = r#"
dnl  comment
dnl  another comment

define(EXAMPLE_SUFFIX,
[$1=`echo $2 | sed -e '/\//s:^[^/]*/::' -e 's:[\\/]:_:g'`])
"#;
    let mut p = make_parser(input);
    dbg!(p.complete_command().unwrap());
}

#[test]
fn test_macro_array_argument() {
    let input = r#"AC_CHECK_DECLS([example_func], [], [], [#include <example.h>])"#;
    let mut p = make_parser(input);
    dbg!(p.complete_command().unwrap());
}

#[test]
fn test_macro_quoted_comma() {
    let input = r#"
  AC_ARG_ENABLE(feature,
  [AS_HELP_STRING([[--enable-feature[=all]]],
		  [choose feature])],
  [feature_val=`echo ,$enable_feature, | sed -e "s/^,//" -e "s/,$//" `])
        "#;
    let mut p = make_parser(input);
    match p.complete_command() {
        Ok(cmd) => {
            dbg!(&cmd);
        }
        Err(e) => {
            println!("{}", e);
            panic!();
        }
    }
}

#[test]
fn test_macro_quoted_positional_param() {
    let input = r#"echo [$][1]"#;
    let mut p = make_parser(input);
    let expected = cmd_from_simple(*cmd_args_simple("echo", &["$1"]));
    assert_eq!(p.complete_command().unwrap(), Some(expected));
}

#[test]
fn test_macro_quoted_variable() {
    let input = r#"test "[$][][condition]" = yes"#;
    let mut p = make_parser(input);
    let expected = cmd_from_simple(*cmd_args_words(
        "test",
        &[double_quoted(&["$condition"]), word("="), word("yes")],
    ));
    assert_eq!(p.complete_command().unwrap(), Some(expected));
}

#[test]
fn test_macro_with_quoted_case_command() {
    let input = r#"AC_CHECK_HEADERS(dummy.h,
    AC_CHECK_LIB(dummy, func,[
        WITH_DUMMY=1
        if test "x${DUMMY_DIR}" != "x"; then
            DUMMY_CFLAGS="-I${DUMMY_DIR}/include"
            DUMMY_LIBS="-L${DUMMY_DIR}/lib -ldummy"
            [case ${host} in
                *-*-example*)
                    DUMMY_LIBS="-L${DUMMY_DIR}/lib -R${DUMMY_DIR}/lib -ldummy"
                    ;;
            esac]
        else
            DUMMY_LIBS="-ldummy"
        fi])
    PKG_LIBS="${PKG_LIBS} ${DUMMY_LIBS}"
    )"#;
    let mut p = make_parser(input);
    match p.complete_command() {
        Ok(cmd) => {
            dbg!(&cmd);
        }
        Err(e) => {
            println!("{}", e);
            panic!();
        }
    }
}
