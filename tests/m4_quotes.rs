#![deny(rust_2018_idioms)]
use autotools_parser::ast::builder::*;
use autotools_parser::ast::minimal::WordFragment::*;
use autotools_parser::ast::Parameter;

mod minimal_util;
use crate::minimal_util::*;

#[test]
fn test_quoted_patterns() {
    let input = r#"case $var in
                  [[a]* | [b]:[c]*)]    ;;  # closing quote after ')'
                  [[a-z]]* | ?:[[b]]* ) ;;  # closing quote inside words
                  [x] | [y])            ;;  # inside words & end with closing quotes
                  [a | b] | [c | d])    ;;  # inside words & end with closing quotes & split '|'
                  "a-[$][cond]")          ;;  # interpolated & quoted variable
                  esac"#;
    let empty_cmds = CommandGroup::<MinimalCommand> {
        commands: vec![],
        trailing_comments: vec![],
    };
    let correct = CaseFragments {
        word: word(param(Parameter::<String>::Var("var".into()))),
        post_word_comments: vec![],
        in_comment: Some(Newline(None)),
        arms: vec![
            CaseArm {
                patterns: CasePatternFragments {
                    pre_pattern_comments: vec![],
                    pattern_alternatives: vec![
                        words(&[SquareOpen, lit("a"), SquareClose, Star]),
                        words(&[
                            SquareOpen,
                            lit("b"),
                            SquareClose,
                            Colon,
                            SquareOpen,
                            lit("c"),
                            SquareClose,
                            Star,
                        ]),
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
                        words(&[SquareOpen, lit("a-z"), SquareClose, Star]),
                        words(&[Question, Colon, SquareOpen, lit("b"), SquareClose, Star]),
                    ],
                    pattern_comment: None,
                },
                body: empty_cmds.clone(),
                arm_comment: Some(Newline(Some("# closing quote inside words".into()))),
            },
            CaseArm {
                patterns: CasePatternFragments {
                    pre_pattern_comments: vec![],
                    pattern_alternatives: vec![word_lit("x"), word_lit("y")],
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
                    pattern_alternatives: vec![
                        word_lit("a"),
                        word_lit("b"),
                        word_lit("c"),
                        word_lit("d"),
                    ],
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
                    pattern_alternatives: vec![word(double_quoted(&[lit("a-"), var("cond")]))],
                    pattern_comment: None,
                },
                body: empty_cmds.clone(),
                arm_comment: Some(Newline(Some("# interpolated & quoted variable".into()))),
            },
        ],
        post_arms_comments: vec![],
    };
    let mut p = make_parser(input);
    let res = p.case_command().unwrap();

    assert_eq!(correct, res);
}
