#![deny(rust_2018_idioms)]
use autoconf_parser::ast::builder::*;
use autoconf_parser::ast::Parameter;
use autoconf_parser::ast::TopLevelCommand;

mod parse_support;
use crate::parse_support::*;

#[test]
fn test_quoted_patterns() {
    let input = r#"case $var in
                  [[a]* | [b]:[c]*)]    ;;  # closing quote after ')'
                  [[a-z]]* | ?:[[b]]* ) ;;  # closing quote inside words
                  [x] | [y])            ;;  # inside words & end with closing quotes
                  [a | b] | [c | d])    ;;  # inside words & end with closing quotes & split '|'
                  "a-[$][cond]")          ;;  # interpolated & quoted variable
                  esac"#;
    let empty_cmds = CommandGroup::<TopLevelCommand<String>> {
        commands: vec![],
        trailing_comments: vec![],
    };
    let correct = CaseFragments {
        word: word_param(Parameter::<String>::Var("var".into())),
        post_word_comments: vec![],
        in_comment: Some(Newline(None)),
        arms: vec![
            CaseArm {
                patterns: CasePatternFragments {
                    pre_pattern_comments: vec![],
                    pattern_alternatives: vec![
                        concat_words(&["[", "a", "]", "*"]),
                        concat_words(&["[", "b", "]", ":", "[", "c", "]", "*"]),
                    ],
                    pattern_comment: None,
                },
                body: empty_cmds.clone(),
                arm_comment: Some(Newline(Some("# closing quote after ')'".into()))),
            },
            CaseArm {
                patterns: CasePatternFragments {
                    pre_pattern_comments: vec![],
                    pattern_alternatives: vec![
                        concat_words(&["[", "a-z", "]", "*"]),
                        concat_words(&["?", ":", "[", "b", "]", "*"]),
                    ],
                    pattern_comment: None,
                },
                body: empty_cmds.clone(),
                arm_comment: Some(Newline(Some("# closing quote inside words".into()))),
            },
            CaseArm {
                patterns: CasePatternFragments {
                    pre_pattern_comments: vec![],
                    pattern_alternatives: vec![word("x"), word("y")],
                    pattern_comment: None,
                },
                body: empty_cmds.clone(),
                arm_comment: Some(Newline(Some(
                    "# inside words & end with closing quotes".into(),
                ))),
            },
            CaseArm {
                patterns: CasePatternFragments {
                    pre_pattern_comments: vec![],
                    pattern_alternatives: vec![word("a"), word("b"), word("c"), word("d")],
                    pattern_comment: None,
                },
                body: empty_cmds.clone(),
                arm_comment: Some(Newline(Some(
                    "# inside words & end with closing quotes & split '|'".into(),
                ))),
            },
            CaseArm {
                patterns: CasePatternFragments {
                    pre_pattern_comments: vec![],
                    pattern_alternatives: vec![double_quoted(&["a-", "$cond"])],
                    pattern_comment: None,
                },
                body: empty_cmds.clone(),
                arm_comment: Some(Newline(Some("# interpolated & quoted variable".into()))),
            },
        ],
        post_arms_comments: vec![],
    };
    let mut p = make_parser(input);
    let res = p.case_command();

    assert_eq!(res, Ok(correct));
}
