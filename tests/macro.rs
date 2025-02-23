#![deny(rust_2018_idioms)]
use autoconf_parser::ast::Command::*;
use autoconf_parser::m4_macro::M4Argument::*;
use autoconf_parser::m4_macro::M4Macro;
use autoconf_parser::ast::PipeableCommand::*;
use autoconf_parser::ast::*;

mod parse_support;
use crate::parse_support::*;

pub fn macro_as_command(name: &str, args: &[&str]) -> TopLevelCommand<String> {
    TopLevelCommand(List(CommandList {
        first: ListableCommand::Single(Compound(Box::new(CompoundCommand {
            kind: CompoundCommandKind::Macro(M4Macro {
                name: name.to_string(),
                args: args.iter().map(|a| Literal(a.to_string())).collect(),
            }),
            io: vec![],
        }))),
        rest: vec![],
    }))
}

#[test]
fn test_macro_call_with_empty_parentheses() {
    let mut p = make_parser("TEST_MACRO()");
    let correct = macro_as_command("TEST_MACRO", &[""]);
    assert_eq!(correct, p.complete_command().unwrap().unwrap());
}

#[test]
fn test_macro_with_raw_literal() {
    let input = r#"TEST_ENABLE([case "${enableval}" in
        yes) feature=true ;;
        no)  feature=false ;;
        *)   AC_MSG_ERROR([bad value ${enableval}]) ;;
    esac])"#;

    let mut p = make_parser(input);
    let correct = macro_as_command(
        "TEST_ENABLE",
        &[r#"case "${enableval}" in
        yes) feature=true ;;
        no)  feature=false ;;
        *)   AC_MSG_ERROR([bad value ${enableval}]) ;;
    esac"#],
    );
    assert_eq!(correct, p.complete_command().unwrap().unwrap());
}

#[test]
fn test_macro_with_command() {
    let input = r#"AH_CONFIG_COMMANDS_PRE([echo hi])"#;
    let mut p = make_parser(input);
    let name = "AH_CONFIG_COMMANDS_PRE";
    let correct = M4Macro {
        name: name.to_string(),
        args: vec![Command(vec![cmd_args("echo", &["hi"])])],
    };
    assert_eq!(correct, p.macro_call(&[name]).unwrap());
}

#[test]
fn test_macro_complex() {
    let input = r#"AC_ARG_ENABLE([feature],
                   [AS_HELP_STRING([--enable-feature], [enable feature])],
                   [case "${enableval}" in
                       yes) feature=true ;;
                       no)  feature=false ;;
                       *)   AC_MSG_ERROR([bad value ${enableval}]) ;;
                    esac],
                   [feature=false])"#;

    let mut p = make_parser(input);
    // let correct = macro_as_command(
    //     "AC_ARG_ENABLE",
    //     &[
    //         "feature",
    //         "AS_HELP_STRING([--enable-feature], [enable feature])",
    //         r#"case "${enableval}" in
    //             yes) feature=true ;;
    //             no)  feature=false ;;
    //             *)   AC_MSG_ERROR([bad value ${enableval}]) ;;
    //          esac"#,
    //         "feature=false",
    //     ],
    // );
    assert!(p.compound_command().is_ok());
}
