#![deny(rust_2018_idioms)]
use autotools_parser::ast::Redirect::Heredoc;
use autotools_parser::ast::{Parameter, ParameterSubstitution};
use autotools_parser::parse::ParseErrorKind::*;
use autotools_parser::token::Token;

mod minimal_util;
use crate::minimal_util::*;

fn cat_heredoc(fd: Option<u16>, body: &'static str) -> MinimalCommand {
    redirect(cmd("cat"), &[Heredoc(fd, word_lit(body))])
}

#[test]
fn test_heredoc_valid() {
    let correct = Some(cat_heredoc(None, "hello\n"));
    assert_eq!(
        correct,
        make_parser("cat <<eof\nhello\neof\n")
            .complete_command()
            .unwrap()
    );
}

#[test]
fn test_heredoc_valid_eof_after_delimiter_allowed() {
    let correct = Some(cat_heredoc(None, "hello\n"));
    assert_eq!(
        correct,
        make_parser("cat <<eof\nhello\neof")
            .complete_command()
            .unwrap()
    );
}

#[test]
fn test_heredoc_valid_with_empty_body() {
    let correct = Some(cat_heredoc(None, ""));
    assert_eq!(
        correct,
        make_parser("cat <<eof\neof").complete_command().unwrap()
    );
    assert_eq!(
        correct,
        make_parser("cat <<eof\n").complete_command().unwrap()
    );
    assert_eq!(
        correct,
        make_parser("cat <<eof").complete_command().unwrap()
    );
}

#[test]
fn test_heredoc_valid_eof_acceptable_as_delimeter() {
    let correct = Some(cat_heredoc(None, "hello\n"));
    assert_eq!(
        correct,
        make_parser("cat <<eof\nhello\neof")
            .complete_command()
            .unwrap()
    );
}

#[test]
fn test_heredoc_valid_does_not_lose_tokens_up_to_next_newline() {
    let mut p = make_parser("cat <<eof1; cat 3<<eof2\nhello\neof1\nworld\neof2");
    let first = Some(cat_heredoc(None, "hello\n"));
    let second = Some(cat_heredoc(Some(3), "world\n"));

    assert_eq!(first, p.complete_command().unwrap());
    assert_eq!(second, p.complete_command().unwrap());
}

#[test]
fn test_heredoc_valid_space_before_delimeter_allowed() {
    let mut p = make_parser("cat <<   eof1; cat 3<<- eof2\nhello\neof1\nworld\neof2");
    let first = Some(cat_heredoc(None, "hello\n"));
    let second = Some(cat_heredoc(Some(3), "world\n"));

    assert_eq!(first, p.complete_command().unwrap());
    assert_eq!(second, p.complete_command().unwrap());
}

#[test]
fn test_heredoc_valid_unquoted_delimeter_should_expand_body() {
    let expanded = Some(redirect(
        cmd("cat"),
        &[Heredoc(
            None,
            words(&[
                param(Parameter::Dollar),
                lit(" "),
                subst(ParameterSubstitution::Len(Parameter::Bang)),
                lit(" "),
                subst(ParameterSubstitution::Command(vec![cmd("foo")])),
                lit("\n"),
            ]),
        )],
    ));

    let literal = Some(cat_heredoc(None, "$$ ${#!} `foo`\n"));

    assert_eq!(
        expanded,
        make_parser("cat <<eof\n$$ ${#!} `foo`\neof")
            .complete_command()
            .unwrap()
    );
    assert_eq!(
        literal,
        make_parser("cat <<'eof'\n$$ ${#!} `foo`\neof")
            .complete_command()
            .unwrap()
    );
    assert_eq!(
        literal,
        make_parser("cat <<`eof`\n$$ ${#!} `foo`\n`eof`")
            .complete_command()
            .unwrap()
    );
    assert_eq!(
        literal,
        make_parser("cat <<\"eof\"\n$$ ${#!} `foo`\neof")
            .complete_command()
            .unwrap()
    );
    assert_eq!(
        literal,
        make_parser("cat <<e\\of\n$$ ${#!} `foo`\neof")
            .complete_command()
            .unwrap()
    );
}

#[test]
fn test_heredoc_valid_leading_tab_removal_works() {
    let mut p =
        make_parser("cat <<-eof1; cat 3<<-eof2\n\t\thello\n\teof1\n\t\t \t\nworld\n\t\teof2");
    let first = Some(cat_heredoc(None, "hello\n"));
    let second = Some(cat_heredoc(Some(3), " \t\nworld\n"));

    assert_eq!(first, p.complete_command().unwrap());
    assert_eq!(second, p.complete_command().unwrap());
}

#[test]
fn test_heredoc_valid_leading_tab_removal_works_if_dash_immediately_after_dless() {
    let mut p = make_parser("cat 3<< -eof\n\t\t \t\nworld\n\t\teof\n\t\t-eof\n-eof");
    let correct = Some(cat_heredoc(Some(3), "\t\t \t\nworld\n\t\teof\n\t\t-eof\n"));
    assert_eq!(correct, p.complete_command().unwrap());
}

#[test]
fn test_heredoc_valid_unquoted_backslashes_in_delimeter_disappear() {
    let correct = Some(cat_heredoc(None, "hello\n"));
    assert_eq!(
        correct,
        make_parser("cat <<e\\ f\\f\nhello\ne ff")
            .complete_command()
            .unwrap()
    );
}

#[test]
fn test_heredoc_valid_balanced_single_quotes_in_delimeter() {
    let correct = Some(cat_heredoc(None, "hello\n"));
    assert_eq!(
        correct,
        make_parser("cat <<e'o'f\nhello\neof")
            .complete_command()
            .unwrap()
    );
}

#[test]
fn test_heredoc_valid_balanced_double_quotes_in_delimeter() {
    let correct = Some(cat_heredoc(None, "hello\n"));
    assert_eq!(
        correct,
        make_parser("cat <<e\"\\o${foo}\"f\nhello\ne\\o${foo}f")
            .complete_command()
            .unwrap()
    );
}

#[test]
fn test_heredoc_valid_balanced_backticks_in_delimeter() {
    let correct = Some(cat_heredoc(None, "hello\n"));
    assert_eq!(
        correct,
        make_parser("cat <<e`\\o\\$\\`\\\\${f}`\nhello\ne`\\o$`\\${f}`")
            .complete_command()
            .unwrap()
    );
}

#[test]
fn test_heredoc_valid_balanced_parens_in_delimeter() {
    let correct = Some(cat_heredoc(None, "hello\n"));
    assert_eq!(
        correct,
        make_parser("cat <<eof(  )\nhello\neof(  )")
            .complete_command()
            .unwrap()
    );
}

#[test]
fn test_heredoc_valid_cmd_subst_in_delimeter() {
    let correct = Some(cat_heredoc(None, "hello\n"));
    assert_eq!(
        correct,
        make_parser("cat <<eof$(  )\nhello\neof$(  )")
            .complete_command()
            .unwrap()
    );
}

#[test]
fn test_heredoc_valid_param_subst_in_delimeter() {
    let correct = Some(cat_heredoc(None, "hello\n"));
    assert_eq!(
        correct,
        make_parser("cat <<eof${  }\nhello\neof${  }")
            .complete_command()
            .unwrap()
    );
}

#[test]
fn test_heredoc_valid_skip_past_newlines_in_single_quotes() {
    let correct = Some(redirect(
        cmd_from_lits("cat", &["\n", "arg"]),
        &[Heredoc(None, word_lit("here\n"))],
    ));
    assert_eq!(
        correct,
        make_parser("cat <<EOF '\n' arg\nhere\nEOF")
            .complete_command()
            .unwrap()
    );
}

#[test]
fn test_heredoc_valid_skip_past_newlines_in_double_quotes() {
    let correct = Some(redirect(
        cmd_from_words("cat", &[word_lit("\n"), word_lit("arg")]),
        &[Heredoc(None, word_lit("here\n"))],
    ));
    assert_eq!(
        correct,
        make_parser("cat <<EOF \"\n\" arg\nhere\nEOF")
            .complete_command()
            .unwrap()
    );
}

#[test]
fn test_heredoc_valid_skip_past_newlines_in_backticks() {
    let correct = Some(redirect(
        cmd_from_words(
            "cat",
            &[
                word(subst(ParameterSubstitution::Command(vec![cmd("echo")]))),
                word_lit("arg"),
            ],
        ),
        &[Heredoc(None, word_lit("here\n"))],
    ));
    assert_eq!(
        correct,
        make_parser("cat <<EOF `echo \n` arg\nhere\nEOF")
            .complete_command()
            .unwrap()
    );
}

#[test]
fn test_heredoc_valid_skip_past_newlines_in_parens() {
    let correct = Some(cat_heredoc(None, "here\n"));
    assert_eq!(
        correct,
        make_parser("cat <<EOF; (foo\n); arg\nhere\nEOF")
            .complete_command()
            .unwrap()
    );
}

#[test]
fn test_heredoc_valid_skip_past_newlines_in_cmd_subst() {
    let correct = Some(redirect(
        cmd_from_words(
            "cat",
            &[
                word(subst(ParameterSubstitution::Command(vec![cmd("foo")]))),
                word_lit("arg"),
            ],
        ),
        &[Heredoc(None, word_lit("here\n"))],
    ));
    assert_eq!(
        correct,
        make_parser("cat <<EOF $(foo\n) arg\nhere\nEOF")
            .complete_command()
            .unwrap()
    );
}

#[test]
fn test_heredoc_valid_skip_past_newlines_in_param_subst() {
    let correct = Some(redirect(
        cmd_from_words(
            "cat",
            &[
                word(subst(ParameterSubstitution::Assign(
                    false,
                    Parameter::Var(String::from("foo")),
                    Some(word_lit("\n")),
                ))),
                word_lit("arg"),
            ],
        ),
        &[Heredoc(None, word_lit("here\n"))],
    ));
    assert_eq!(
        correct,
        make_parser("cat <<EOF ${foo=\n} arg\nhere\nEOF")
            .complete_command()
            .unwrap()
    );
}

#[test]
fn test_heredoc_valid_skip_past_escaped_newlines() {
    let correct = Some(redirect(
        cmd_from_words("cat", &[word_lit("arg")]),
        &[Heredoc(None, word_lit("here\n"))],
    ));
    assert_eq!(
        correct,
        make_parser("cat <<EOF \\\n arg\nhere\nEOF")
            .complete_command()
            .unwrap()
    );
}

#[test]
fn test_heredoc_valid_double_quoted_delim_keeps_backslashe_except_after_specials() {
    let correct = Some(cat_heredoc(None, "here\n"));
    assert_eq!(
        correct,
        make_parser("cat <<\"\\EOF\\$\\`\\\"\\\\\"\nhere\n\\EOF$`\"\\\n")
            .complete_command()
            .unwrap()
    );
}

#[test]
fn test_heredoc_valid_unquoting_only_removes_outer_quotes_and_backslashes() {
    let correct = Some(cat_heredoc(None, "here\n"));
    assert_eq!(
        correct,
        make_parser("cat <<EOF${ 'asdf'}(\"hello'\"){\\o}\nhere\nEOF${ asdf}(hello'){o}")
            .complete_command()
            .unwrap()
    );
}

#[test]
fn test_heredoc_valid_delimeter_can_be_followed_by_carriage_return_newline() {
    let correct = Some(redirect(
        cmd_from_words("cat", &[word_lit("arg")]),
        &[Heredoc(None, word_lit("here\n"))],
    ));
    assert_eq!(
        correct,
        make_parser("cat <<EOF arg\nhere\nEOF\r\n")
            .complete_command()
            .unwrap()
    );

    let correct = Some(redirect(
        cmd_from_words("cat", &[word_lit("arg")]),
        &[Heredoc(None, word_lit("here\r\n"))],
    ));
    assert_eq!(
        correct,
        make_parser("cat <<EOF arg\nhere\r\nEOF\r\n")
            .complete_command()
            .unwrap()
    );
}

#[test]
fn test_heredoc_valid_delimiter_can_start_with() {
    let correct = Some(cat_heredoc(None, "\thello\n\t\tworld\n"));
    assert_eq!(
        correct,
        make_parser("cat << -EOF\n\thello\n\t\tworld\n-EOF")
            .complete_command()
            .unwrap()
    );

    let correct = Some(cat_heredoc(None, "hello\nworld\n"));
    assert_eq!(
        correct,
        make_parser("cat <<--EOF\n\thello\n\t\tworld\n-EOF")
            .complete_command()
            .unwrap()
    );
}

#[test]
fn test_heredoc_invalid_missing_delimeter() {
    assert_eq!(
        Err(Unexpected(Token::Semi, src(7, 1, 8)).into()),
        make_parser("cat << ;").complete_command()
    );
}

#[test]
fn test_heredoc_invalid_unbalanced_quoting() {
    assert_eq!(
        Err(Unmatched(Token::SingleQuote, src(6, 1, 7)).into()),
        make_parser("cat <<'eof").complete_command()
    );
    assert_eq!(
        Err(Unmatched(Token::Backtick, src(6, 1, 7)).into()),
        make_parser("cat <<`eof").complete_command()
    );
    assert_eq!(
        Err(Unmatched(Token::DoubleQuote, src(6, 1, 7)).into()),
        make_parser("cat <<\"eof").complete_command()
    );
    assert_eq!(
        Err(Unmatched(Token::ParenOpen, src(9, 1, 10)).into()),
        make_parser("cat <<eof(").complete_command()
    );
    assert_eq!(
        Err(Unmatched(Token::ParenOpen, src(10, 1, 11)).into()),
        make_parser("cat <<eof$(").complete_command()
    );
    assert_eq!(
        Err(Unmatched(Token::CurlyOpen, src(10, 1, 11)).into()),
        make_parser("cat <<eof${").complete_command()
    );
}

#[test]
fn test_heredoc_invalid_shows_right_position_of_error() {
    let mut p = make_parser("cat <<EOF\nhello\n${invalid subst\nEOF");
    assert_eq!(
        Err(BadSubst(Token::Whitespace(String::from(" ")), src(25, 3, 10)).into()),
        p.complete_command()
    );
}

#[test]
fn test_heredoc_invalid_shows_right_position_of_error_when_tabs_stripped() {
    let mut p = make_parser("cat <<-EOF\n\t\thello\n\t\t${invalid subst\n\t\t\tEOF");
    assert_eq!(
        Err(BadSubst(Token::Whitespace(String::from(" ")), src(30, 3, 12)).into()),
        p.complete_command()
    );
}

#[test]
fn test_heredoc_keeps_track_of_correct_position_after_redirect() {
    let mut p = make_parser("cat <<EOF arg ()\nhello\nEOF");
    // Grab the heredoc command
    p.complete_command().unwrap();
    // Fail on the ()
    assert_eq!(
        Err(Unexpected(Token::ParenClose, src(15, 1, 16)).into()),
        p.complete_command()
    );
}
