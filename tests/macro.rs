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
    dbg!(p.compound_command().unwrap());
}

#[test]
fn test_macro_with_trailing_comments() {
    let input = r#"
dnl  GMP_FAT_SUFFIX(DSTVAR, DIRECTORY)
dnl  ---------------------------------
dnl  Emit code to set shell variable DSTVAR to the suffix for a fat binary
dnl  routine from DIRECTORY.  DIRECTORY can be a shell expression like $foo
dnl  etc.
dnl
dnl  The suffix is directory separators / or \ changed to underscores, and
dnl  if there's more than one directory part, then the first is dropped.
dnl
dnl  For instance,
dnl
dnl      x86         ->  x86
dnl      x86/k6      ->  k6
dnl      x86/k6/mmx  ->  k6_mmx

define(GMP_FAT_SUFFIX,
[[$1=`echo $2 | sed -e '/\//s:^[^/]*/::' -e 's:[\\/]:_:g'`]])

"#;
    let mut p = make_parser(input);
    dbg!(p.complete_command().unwrap());
}

#[test]
fn test_macro_unusual_style_of_newline() {
    let input = r#"dnl  GMP_VERSION
dnl  -----------
dnl  The gmp version number, extracted from the #defines in gmp-h.in at
dnl  autoconf time.  Two digits like 3.0 if patchlevel <= 0, or three digits
dnl  like 3.0.1 if patchlevel > 0.

define(GMP_VERSION,
[GMP_HEADER_GETVAL(__GNU_MP_VERSION,gmp-h.in)[]dnl
.GMP_HEADER_GETVAL(__GNU_MP_VERSION_MINOR,gmp-h.in)[]dnl
.GMP_HEADER_GETVAL(__GNU_MP_VERSION_PATCHLEVEL,gmp-h.in)])
"#;
    let mut p = make_parser(input);
    dbg!(p.complete_command().unwrap());
}

#[test]
fn test_macro_t() {
    let input =
        r#"GMP_DEFINE_RAW("define_not_for_expansion(\`HAVE_DOUBLE_IEEE_BIG_ENDIAN')", POST)"#;
    let mut p = make_parser(input);
    dbg!(p.complete_command().unwrap());
}
