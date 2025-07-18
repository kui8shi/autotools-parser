//! The definition of a parser (and related methods) for the shell language.
// FIXME: consider parsing [[ exr ]] as keywords? otherwise [[ foo && bar ]] won't get parsed right
// FIXME: consider parsing out array index syntax? (e.g. ${array[some index]}
// FIXME: arithmetic substitutions don't currently support param/comand substitutions

use std::backtrace::{Backtrace, BacktraceStatus};
use std::convert::From;
use std::error::Error;
use std::fmt;

use crate::token::Token;
use crate::token::Token::*;

pub mod autoconf;
pub mod automake;
pub(crate) mod iter;

const CASE: &str = "case";
const DO: &str = "do";
const DONE: &str = "done";
const ELIF: &str = "elif";
const ELSE: &str = "else";
const ESAC: &str = "esac";
const FI: &str = "fi";
const FOR: &str = "for";
const FUNCTION: &str = "function";
const IF: &str = "if";
const IN: &str = "in";
const THEN: &str = "then";
const UNTIL: &str = "until";
const WHILE: &str = "while";

/// A specialized `Result` type for parsing shell commands.
pub type ParseResult<T, E> = Result<T, ParseError<E>>;

/// Indicates a character/token position in the original source.
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub struct SourcePos {
    /// The byte offset since the start of parsing.
    pub byte: usize,
    /// The line offset since the start of parsing, useful for error messages.
    pub line: usize,
    /// The column offset since the start of parsing, useful for error messages.
    pub col: usize,
}

impl Default for SourcePos {
    fn default() -> Self {
        Self::new()
    }
}

impl SourcePos {
    /// Constructs a new, starting, source position
    pub fn new() -> SourcePos {
        SourcePos {
            byte: 0,
            line: 1,
            col: 1,
        }
    }

    /// Increments self using the length of the provided token.
    pub fn advance(&mut self, next: &Token) {
        let newlines = match *next {
            // Most of these should not have any newlines
            // embedded within them, but permitting external
            // tokenizers means we should sanity check anyway.
            Name(ref s) | Literal(ref s) | Whitespace(ref s) => {
                s.chars().filter(|&c| c == '\n').count()
            }

            Newline => 1,
            _ => 0,
        };

        let tok_len = next.len();
        self.byte += tok_len;
        self.line += newlines;
        self.col = if newlines == 0 { self.col + tok_len } else { 1 };
    }

    /// Increments self by `num_tab` tab characters
    fn advance_tabs(&mut self, num_tab: usize) {
        self.byte += num_tab;
        self.col += num_tab;
    }
}

/// Wrapper type of `ParseErrorKind` to manage backtrace.
#[derive(Debug)]
pub struct ParseError<T> {
    /// Internal error field.
    pub kind: ParseErrorKind<T>,
    /// Take backtrace when the error happened.
    backtrace: Backtrace,
}

impl<T> ParseError<T> {
    /// Create a new ParseError from the error kind and captured backtrace.
    pub fn new(kind: ParseErrorKind<T>) -> Self {
        ParseError {
            kind,
            backtrace: Backtrace::capture(),
        }
    }

    /// Return reference of backtrace.
    pub fn backtrace(&self) -> &Backtrace {
        &self.backtrace
    }
}

impl<T> From<ParseErrorKind<T>> for ParseError<T> {
    fn from(value: ParseErrorKind<T>) -> Self {
        Self::new(value)
    }
}

impl<T> PartialEq for ParseError<T>
where
    T: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

use ParseErrorKind::*;

/// The error type which is returned from parsing shell commands.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseErrorKind<T> {
    /// Encountered a word that could not be interpreted as a valid file descriptor.
    /// Stores the start and end position of the invalid word.
    BadFd(SourcePos, SourcePos),
    /// Encountered a `Token::Literal` where expecting a `Token::Name`.
    BadIdent(String, SourcePos),
    /// Encountered a bad token inside of `${...}`.
    BadSubst(Token, SourcePos),
    /// Encountered EOF while looking for a match for the specified token.
    /// Stores position of opening token.
    Unmatched(Token, SourcePos),
    /// Did not find a reserved keyword within a command. The first String is the
    /// command being parsed, followed by the position of where it starts. Next
    /// is the missing keyword followed by the position of where the parse
    /// expected to have encountered it.
    IncompleteCmd(&'static str, SourcePos, &'static str, SourcePos),
    /// Encountered a token not appropriate for the current context.
    Unexpected(Token, SourcePos),
    /// Encountered the end of input while expecting additional tokens.
    UnexpectedEOF,
    /// A custom error returned by the AST builder.
    Custom(T),
}

impl<T: Error + 'static> Error for ParseError<T> {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match &self.kind {
            Custom(e) => Some(e),
            _ => None,
        }
    }
    // FIXME(breaking): change this to be `source`, breaking because it
    // would require a new 'static bound on T
    // fn cause(&self) -> Option<&dyn Error> {
    //     match *self.kind {
    //         ParseErrorKind::BadFd(..)
    //         | ParseErrorKind::BadIdent(..)
    //         | ParseErrorKind::BadSubst(..)
    //         | ParseErrorKind::Unmatched(..)
    //         | ParseErrorKind::IncompleteCmd(..)
    //         | ParseErrorKind::Unexpected(..)
    //         | ParseErrorKind::UnexpectedEOF => None,
    //         ParseErrorKind::Custom(ref e) => Some(e),
    //     }
    // }
}

impl<T: fmt::Display> fmt::Display for ParseError<T> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            BadFd(ref start, ref end) => write!(
                fmt,
                "file descriptor found between lines {} - {} cannot possibly be a valid",
                start, end
            ),
            BadIdent(ref id, pos) => {
                write!(fmt, "not a valid identifier {}: {}", pos, id)
            }
            BadSubst(ref t, pos) => {
                write!(fmt, "bad substitution {}: invalid token: {}", pos, t)
            }
            Unmatched(ref t, pos) => {
                write!(fmt, "unmatched `{}` starting on line {}", t, pos)
            }

            IncompleteCmd(c, start, kw, kw_pos) => write!(
                fmt,
                "did not find `{}` keyword on line {}, in `{}` command which starts on line {}",
                kw, kw_pos, c, start
            ),

            // When printing unexpected newlines, print \n instead to avoid confusingly formatted messages
            Unexpected(Newline, pos) => {
                write!(fmt, "found unexpected token on line {}: \\n", pos)
            }
            Unexpected(ref t, pos) => {
                write!(fmt, "found unexpected token on line {}: {}", pos, t)
            }

            UnexpectedEOF => fmt.write_str("unexpected end of input"),
            Custom(ref e) => write!(fmt, "{}", e),
        }?;
        if self.backtrace().status() == BacktraceStatus::Captured {
            write!(fmt, "\nBacktrace:\n{}", self.backtrace())
        } else {
            Ok(())
        }
    }
}

impl<T> From<T> for ParseError<T> {
    fn from(err: T) -> Self {
        ParseError::new(Custom(err))
    }
}

impl fmt::Display for SourcePos {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "{}:{}", self.line, self.col)
    }
}

/// Used to configure when `Parser::command_group` stops parsing commands.
#[derive(Debug, PartialEq, Eq, Clone, Default)]
pub struct CommandGroupDelimiters<'a, 'b, 'c> {
    /// Any token which appears after a complete command separator (e.g. `;`, `&`, or a
    /// newline) will be considered a delimeter for the command group.
    pub reserved_tokens: &'a [Token],
    /// Any `Literal` or `Name` token that matches any of these entries completely
    /// *and* appear after a complete command will be considered a delimeter.
    pub reserved_words: &'b [&'static str],
    /// Any token which matches this provided set will be considered a delimeter.
    pub exact_tokens: &'c [Token],
}

/// dummy trait for `ParserIterator`.
pub trait Parser {
    /// Top-Level AST node type
    type TopLevel;
    /// AST builder's error type
    type Error;
    /// Entrypoint of parsing Top-Level AST node
    fn entry(&mut self) -> ParseResult<Option<Self::TopLevel>, Self::Error>;
}

/// An `Iterator` adapter around a `Parser`.
///
/// This iterator is `fused`, that is, if the underlying parser either yields
/// no command, or an error, no further commands (or errors) will be yielded.
///
/// This is because the parser does not do any error handling or backtracking
/// on errors, thus trying to parse another command after an error is not
/// well defined, and will either fail as well, or will produce an incorrect
/// result.
#[must_use = "iterator adaptors are lazy and do nothing unless consumed"]
#[derive(Debug)]
pub struct ParserIterator<P> {
    /// The underlying parser to poll for complete commands.
    /// A `None` value indicates the stream has been exhausted.
    parser: Option<P>,
}

impl<P> ParserIterator<P> {
    /// Construct a new adapter with a given parser.
    fn new(parser: P) -> Self {
        ParserIterator {
            parser: Some(parser),
        }
    }
}

impl<P: Parser> std::iter::FusedIterator for ParserIterator<P> {}

impl<P: Parser> Iterator for ParserIterator<P> {
    type Item = ParseResult<P::TopLevel, P::Error>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.parser.as_mut().map(P::entry) {
            None => None,
            Some(ret) => match ret {
                Ok(Some(c)) => Some(Ok(c)),
                Ok(None) => {
                    let _ = self.parser.take();
                    None
                }
                Err(e) => {
                    let _ = self.parser.take();
                    Some(Err(e))
                }
            },
        }
    }
}
