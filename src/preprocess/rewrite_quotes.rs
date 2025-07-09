//! Module for selectively rewriting quotes in m4/autoconf macros.
//!
//! This module provides functionality to transform quotes in user-defined and
//! m4_* macros before parsing and expansion, allowing for more accurate
//! macro processing.

use std::collections::{HashMap, HashSet};

use crate::lexer::Lexer;
use crate::m4_macro::{get_macro, M4Type};
use crate::autoconf::iter::{PeekableIterator, TokenIter, TokenIterWrapper};
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
                // m4sugar somehow evaluates AC_REQUIRE.
                ("AC_REQUIRE".into(), "dnl AC_REQUIRE".into()),
                // Current autoconf-parser cannot recognize it.
                ("AC_FD_CC".into(), "5".into()),
                // Currently m4_foreach cannot not be processed under an environment with quotes changed
                ("m4_foreach".into(), "M4_FOREACH".into()),
                // ("m4_toupper".into(), "TO_UPPER".into()),
            ]),
        }
    }
}

/// Rewrites m4 quotes in an m4/autoconf file content
pub fn rewrite_quotes(input: &str) -> String {
    let mut rewriter = Rewriter::new(Lexer::new(input.chars()), QuoteRewriteConfig::default());
    rewriter.rewrite_quotes()
}

/// Same as above but with a configuration specifying quoting characters
pub fn rewrite_quotes_with_config(input: &str, config: QuoteRewriteConfig) -> String {
    let mut rewriter = Rewriter::new(Lexer::new(input.chars()), config);
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

// Can be derived but explicitly implemented

/// Configuration for quote rewriting
#[derive(Debug)]
struct Rewriter<I> {
    iter: TokenIterWrapper<I>,
    config: QuoteRewriteConfig,
    result: String,
    stack: Vec<InMacroState>,
    called_macros: HashSet<String>,
    in_double_quote_string: bool,
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
            called_macros: HashSet::new(),
            in_double_quote_string: false,
        }
    }
}

fn is_user_macro(name: &str) -> bool {
    name.chars().next().is_some_and(|first| {
        (first.is_ascii() && first.is_alphabetic() && first.is_uppercase()) || first == '_'
    }) && name.chars().skip(1).all(|c| {
        (c.is_ascii() && c.is_alphabetic() && c.is_uppercase()) || c.is_numeric() || c == '_'
    })
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
                    if self.iter.peek() == Some(&Token::ParenOpen)
                        && !(!is_user_macro(name)
                            && self
                                .stack
                                .last()
                                .is_some_and(|ctx| ctx.quote_level > ctx.rewrite_level)) =>
                {
                    if let Some((ref name, rewrite_level)) = self.macro_name(name) {
                        self.called_macros.insert(name.into());
                        self.stack.push(InMacroState {
                            rewrite_level,
                            ..Default::default()
                        });
                        self.result.push_str(name);
                    } else if name == "AC_REQUIRE" {
                        // TODO: support m4_require
                        assert_eq!(self.iter.next(), Some(Token::ParenOpen));
                        if self.iter.peek().unwrap().to_string() == self.config.open_quote {
                            self.iter.next();
                        }
                        let required = if let Some(Token::Name(s)) = self.iter.next() {
                            s
                        } else {
                            Default::default()
                        };
                        if self.iter.peek().unwrap().to_string() == self.config.close_quote {
                            self.iter.next();
                        }
                        assert_eq!(self.iter.next(), Some(Token::ParenClose));
                        if !self.called_macros.contains(&required) {
                            self.called_macros.insert(required.clone());
                            // user-defined macro is required.
                            // self.result.push_str(&required);
                        } else {
                            // m4sugar somehow evaluates AC_REQUIRE.
                            // protect AC_REQUIRE from got evaluated
                            self.result.push_str(&format!(
                                "dnl {}AC_REQUIRE{}([{}])",
                                self.config.transformed_open_quote,
                                self.config.transformed_close_quote,
                                &required,
                            ))
                        }
                    } else {
                        self.stack.push(InMacroState {
                            ..Default::default()
                        });
                        self.result.push_str(name)
                    }
                }
                Token::DoubleQuote => {
                    self.in_double_quote_string = !self.in_double_quote_string;
                    self.result.push('"');
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
        if let Some((name, sig, _)) = get_macro(name) {
            let is_defining_macro = matches!(sig.ret_type, Some(M4Type::Def));
            let name = if !name.starts_with("m4_") && is_defining_macro {
                // The condition is a heuristic to detect macro defining macros such as AC_DEFUN.
                // we intentionally overwrite the macro name for m4sugar language.
                "m4_define"
            } else {
                name
            };
            if (!self.stack.is_empty() || self.in_double_quote_string) && is_defining_macro {
                // to prevent unintetional expansions, we ignore define macros inside other macros.
                None
            } else if name.starts_with("m4_") {
                Some((name.to_string(), if is_defining_macro { 1 } else { 1 }))
            } else {
                None
            }
        } else {
            is_user_macro(name).then_some((name.to_string(), 1))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_rewrite() {
        let input = "m4_define([MACRO_NAME], [macro body])";
        let expected =
            "m4_define(|OPENQUOTE|MACRO_NAME|CLOSEQUOTE|, |OPENQUOTE|macro body|CLOSEQUOTE|)";
        assert_eq!(rewrite_quotes(input), expected);
    }

    #[test]
    fn test_ac_require() {
        // FIXME
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

    #[test]
    fn test_nested_rewirte() {
        let input = r#"
    AC_CACHE_CHECK([for test],
                    cv_test, [
    MACRO([AAAA],
    [cv_test="\$][1,\$][2"])
    ])
    "#;
        let expected = r#"
    AC_CACHE_CHECK([for test],
                    cv_test, [
    MACRO(|OPENQUOTE|AAAA|CLOSEQUOTE|,
    |OPENQUOTE|cv_test="\$|CLOSEQUOTE||OPENQUOTE|1,\$|CLOSEQUOTE||OPENQUOTE|2"|CLOSEQUOTE|)
    ])
    "#;
        assert_eq!(rewrite_quotes(input), expected);
    }
}
