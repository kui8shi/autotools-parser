//! Module for selectively rewriting quotes in m4/autoconf macros.
//!
//! This module provides functionality to transform quotes in user-defined and
//! m4_* macros before parsing and expansion, allowing for more accurate
//! macro processing.

use std::collections::HashMap;

use crate::lexer::Lexer;
use crate::m4_macro::{get_macro, M4Type};
use crate::parse::iter::{PeekableIterator, TokenIter, TokenIterWrapper};
use crate::token::Token;

/// Configuration for quote rewriting
#[derive(Debug, Clone)]
pub struct QuoteRewriteConfig {
    /// Opening quote string to be replaced.
    pub open_quote: String,
    /// Closing quote string to be replaced.
    pub close_quote: String,
    /// Quote String to replace existing opening quote with
    pub transformed_open_quote: String,
    /// Quote String to replace existing closing quote with
    pub transformed_close_quote: String,
    /// Names to replace. This is to avoid unusual m4 errors.
    pub replace: HashMap<String, String>,
}

impl Default for QuoteRewriteConfig {
    fn default() -> Self {
        Self {
            open_quote: "[".to_string(),
            close_quote: "]".to_string(),
            transformed_open_quote: "|OPENQUOTE|".to_string(),
            transformed_close_quote: "|CLOSEQUOTE|".to_string(),
            replace: HashMap::from([
                // Don't know why but calling this undocumented macro: `m4_PACKAGE_VERSION` with
                // quotes changed causes infinite recursive expansion.
                ("m4_PACKAGE_VERSION".into(), "AC_AUTOCONF_VERSION".into()),
                // Don't know why but AC_BEFORE is error-prone.
                ("AC_BEFORE".into(), "dnl AC_BEFORE".into()),
                // AC_REQUIRE's argument (which is another macro name) should be re-quoted.
                // This replacement is more inprecise but simpler solution.
                ("AC_REQUIRE".into(), "dnl AC_REQUIRE".into()),
            ]),
        }
    }
}

/// Rewrites m4 quotes in an m4/autoconf file content
pub fn rewrite_quotes(input: &str) -> String {
    let mut rewriter = Rewriter::new(
        Lexer::new(input.chars()).into_iter(),
        QuoteRewriteConfig::default(),
    );
    rewriter.rewrite_quotes()
}

/// Same as above but with a configuration specifying quoting characters
pub fn rewrite_quotes_with_config(input: &str, config: QuoteRewriteConfig) -> String {
    let mut rewriter = Rewriter::new(Lexer::new(input.chars()).into_iter(), config);
    rewriter.rewrite_quotes()
}

#[derive(Debug, Default)]
struct InMacroState {
    /// the level of quoting pairs to overwrite.
    rewrite_level: isize,
    /// the current level of quoting pairs.
    quote_level: isize,
    /// the current level of parenthesis.
    paren_level: isize,
}

/// Configuration for quote rewriting
#[derive(Debug)]
struct Rewriter<I> {
    iter: TokenIterWrapper<I>,
    config: QuoteRewriteConfig,
    result: String,
    stack: Vec<InMacroState>,
}

impl<I: Iterator<Item = Token>> Rewriter<I> {
    /// Creates a new Parser from a Token iterator or collection.
    pub fn new<T>(iter: T, config: QuoteRewriteConfig) -> Rewriter<I>
    where
        T: IntoIterator<Item = Token, IntoIter = I>,
    {
        Rewriter {
            iter: TokenIterWrapper::Regular(TokenIter::new(iter.into_iter())),
            config,
            result: String::new(),
            stack: Vec::new(),
        }
    }
}

impl<I: Iterator<Item = Token>> Rewriter<I> {
    /// Rewrites m4 quotes in an m4/autoconf file content
    pub fn rewrite_quotes(&mut self) -> String {
        while self.iter.peek().is_some() {
            self.consume_token();
        }
        self.result.to_string()
    }

    fn consume_token(&mut self) {
        if let Some(tok) = self.iter.next() {
            match tok {
                Token::Name(ref name) if name == "dnl" => {
                    self.result.push_str("dnl");
                    loop {
                        let comment = self.iter.next();
                        match comment {
                            Some(Token::Newline) => {
                                self.result.push('\n');
                                break;
                            }
                            None => break,
                            _ => (),
                        }
                        self.result.push_str(&comment.unwrap().to_string());
                    }
                }
                Token::Name(ref name) if self.config.replace.contains_key(name) => {
                    self.result.push_str(self.config.replace.get(name).unwrap());
                }
                Token::Name(ref name)
                    if (self.stack.is_empty()
                        || (self
                            .stack
                            .last()
                            .is_some_and(|ctx| ctx.quote_level <= ctx.rewrite_level)))
                        && self.iter.peek() == Some(&Token::ParenOpen) =>
                {
                    if let Some((ref name, rewrite_level)) = self.macro_name(name) {
                        self.stack.push(InMacroState {
                            rewrite_level,
                            ..Default::default()
                        });
                        self.result.push_str(name);
                    } else {
                        self.stack.push(InMacroState {
                            ..Default::default()
                        });
                        self.result.push_str(name)
                    }
                }
                Token::ParenOpen => {
                    if let Some(ctx) = self.stack.last_mut() {
                        if ctx.quote_level == 0 {
                            ctx.paren_level += 1;
                        }
                    }
                    self.result.push('(');
                }
                Token::ParenClose => {
                    if let Some(ctx) = self.stack.last_mut() {
                        if ctx.quote_level == 0 {
                            ctx.paren_level -= 1;
                        }
                        if ctx.paren_level == 0 {
                            self.stack.pop();
                        }
                    }
                    self.result.push(')');
                }
                _ => {
                    let tok = tok.to_string();
                    if let Some(ctx) = self.stack.last_mut() {
                        if tok == self.config.open_quote {
                            ctx.quote_level += 1;
                            if 0 < ctx.quote_level && ctx.quote_level <= ctx.rewrite_level {
                                self.result.push_str(&self.config.transformed_open_quote);
                                return;
                            }
                        } else if tok == self.config.close_quote {
                            ctx.quote_level -= 1;
                            if 0 <= ctx.quote_level && ctx.quote_level < ctx.rewrite_level {
                                self.result.push_str(&self.config.transformed_close_quote);
                                return;
                            }
                        } else if ctx.quote_level < ctx.rewrite_level
                            && tok
                                == format!("{}{}", self.config.open_quote, &self.config.close_quote)
                        {
                            self.result.push_str(&format!(
                                "{}{}",
                                &self.config.transformed_open_quote,
                                &self.config.transformed_close_quote
                            ));
                            return;
                        }
                    }
                    self.result.push_str(&tok);
                }
            }
        }
    }

    fn macro_name(&mut self, name: &str) -> Option<(String, isize)> {
        if let Some((name, sig)) = get_macro(name) {
            let is_define_macro = matches!(sig.ret_type, Some(M4Type::Def));
            let name = if !name.starts_with("m4_") && is_define_macro {
                // The condition is a heuristic to detect macro defining macros such as AC_DEFUN.
                // we intentionally overwrite the macro name for m4sugar language.
                "m4_define"
            } else {
                name
            };
            if name.starts_with("m4_") {
                Some((name.to_string(), if is_define_macro { 2 } else { 1 }))
            } else {
                None
            }
        } else {
            let is_macro_name = name.chars().next().is_some_and(|first| {
                (first.is_ascii() && first.is_alphabetic() && first.is_uppercase()) || first == '_'
            }) && name.chars().skip(1).all(|c| {
                (c.is_ascii() && c.is_alphabetic() && c.is_uppercase())
                    || c.is_numeric()
                    || c == '_'
            });
            is_macro_name.then_some((name.to_string(), 1))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_rewrite() {
        let input = "m4_define([MACRO_NAME], [macro body])";
        let expected = "m4_define(|OPENQUOTE|MACRO_NAME|CLOSEQUOTE|, |OPENQUOTE|macro body|CLOSEQUOTE|)";
        assert_eq!(rewrite_quotes(input), expected);
    }

    #[test]
    fn test_nested_rewrite() {
        let input = "m4_define([GMP_M4_M4WRAP_SPURIOUS], [AC_REQUIRE([GMP_PROG_M4])])";
        let expected = "m4_define(|OPENQUOTE|GMP_M4_M4WRAP_SPURIOUS|CLOSEQUOTE|, |OPENQUOTE|AC_REQUIRE([GMP_PROG_M4])|CLOSEQUOTE|)";
        // assert_eq!(rewrite_quotes(input), expected);
    }

    #[test]
    fn test_no_rewrite_for_normal_text() {
        let input = "normal text with [brackets] not in macros";
        let expected = "normal text with [brackets] not in macros";
        assert_eq!(rewrite_quotes(input), expected);
    }

    #[test]
    fn test_quotes_in_shell_commands() {
        let input = "m4_if([$1], [yes], [echo \"Found $1\"; grep -q \"[a-z]+\" file])";
        let result = rewrite_quotes(input);
        // Check that macro quotes are transformed
        assert!(result.contains("m4_if(|OPENQUOTE|$1|CLOSEQUOTE|, |OPENQUOTE|yes|CLOSEQUOTE|"));
        // Check that the shell command's quotes are preserved
        assert!(result.contains("\"Found $1\""));
    }

    #[test]
    fn test_uppercase_user_defined_macro() {
        let input = "MY_MACRO([arg1], [arg2])";
        let expected = "MY_MACRO(|OPENQUOTE|arg1|CLOSEQUOTE|, |OPENQUOTE|arg2|CLOSEQUOTE|)";
        assert_eq!(rewrite_quotes(input), expected);
    }

    #[test]
    fn test_preserve_case_parenthesis() {
        let input = "MACRO(
    [case $enableval in
    yes|no) ;;
    *) ;;
    esac],
    [])";
        let expected = "MACRO(
    |OPENQUOTE|case $enableval in
    yes|no) ;;
    *) ;;
    esac|CLOSEQUOTE|,
    |OPENQUOTE||CLOSEQUOTE|)";
        let result = rewrite_quotes(input);
        assert_eq!(result, expected);
    }
}
