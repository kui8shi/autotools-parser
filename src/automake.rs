use std::backtrace::{Backtrace, BacktraceStatus};
use std::convert::From;
use std::error::Error;
use std::fmt;
use std::iter::empty as empty_iter;
use std::mem;
use std::ops::{AddAssign, Not, SubAssign};
use std::str::FromStr;

use self::iter::{PeekableIterator, PositionIterator, TokenIter, TokenIterWrapper, TokenIterator};
use crate::ast::builder::ConcatWordKind::{self, Concat, Single};
use crate::ast::builder::QuoteWordKind::{DoubleQuoted, Simple, SingleQuoted};
use crate::ast::builder::{self, M4Builder, ShellBuilder, WordKind};
use crate::ast::node::{Node, NodeId};
use crate::ast::{self, DefaultArithmetic, DefaultParameter};
use crate::m4_macro::{self, ArrayDelim, M4Argument, M4ExportFunc, SideEffect};
use crate::token::Token;
use crate::token::Token::*;

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

/// A parser which will use a default AST builder implementation,
/// yielding results in terms of types defined in the `ast` module.
pub type DefaultParser<I> = AutoconfParser<I, builder::StringBuilder>;

/// A parser which will use a minimal set of AST builder implementation.
pub type MinimalParser<I> = AutoconfParser<I, builder::MinimalBuilder<String>>;

/// A parser which will use a node-based AST builder implementation.
pub type NodeParser<I> = AutoconfParser<I, builder::NodeBuilder<String>>;

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

/// Used to indicate what kind of compound command could be parsed next.
#[derive(Debug, PartialEq, Eq, Clone)]
enum CompoundCmdKeyword {
    For,
    Case,
    If,
    While,
    Until,
    Brace,
    Subshell,
    Macro(String),
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
pub struct ParserIterator<I, B> {
    /// The underlying parser to poll for complete commands.
    /// A `None` value indicates the stream has been exhausted.
    parser: Option<AutoconfParser<I, B>>,
}

impl<I, B> ParserIterator<I, B> {
    /// Construct a new adapter with a given parser.
    fn new(parser: AutoconfParser<I, B>) -> Self {
        ParserIterator {
            parser: Some(parser),
        }
    }
}

impl<I, B> std::iter::FusedIterator for ParserIterator<I, B>
where
    I: Iterator<Item = Token>,
    B: ShellBuilder + M4Builder,
    B::WordFragment: Into<Option<String>> + Clone,
{
}


impl<I, B> IntoIterator for AutoconfParser<I, B>
where
    I: Iterator<Item = Token>,
    B: ShellBuilder + M4Builder,
    B::WordFragment: Into<Option<String>> + Clone,
{
    type IntoIter = ParserIterator<I, B>;
    type Item = <Self::IntoIter as Iterator>::Item;

    fn into_iter(self) -> Self::IntoIter {
        ParserIterator::new(self)
    }
}

/// A parser for the shell language. It will parse shell commands from a
/// stream of shell `Token`s, and pass them to an AST builder.
///
/// The parser implements the `IntoIterator` trait so that it can behave like
/// a stream of parsed shell commands. Converting the parser into an `Iterator`
/// and calling `next()` on the result will yield a complete shell command, or
/// an error should one arise.
///
/// # Building
///
/// To construct a parser you need a stream of `Token`s and a `Builder`
/// which will receive data from the parser and assemble an AST. This
/// library provides both a default `Token` lexer, as well as an AST `Builder`.
///
/// ```
/// use autoconf_parser::ast::builder::{Builder, RcBuilder};
/// use autoconf_parser::lexer::Lexer;
/// use autoconf_parser::parse::Parser;
///
/// let source = "echo hello world";
/// let lexer = Lexer::new(source.chars());
/// let mut parser = Parser::with_builder(lexer, RcBuilder::new(), false);
/// assert!(parser.complete_command().unwrap().is_some());
/// ```
///
/// If you want to use a parser with the default AST builder implementation
/// you can also use the `DefaultParser` type alias for a simpler setup.
///
/// ```
/// use autoconf_parser::lexer::Lexer;
/// use autoconf_parser::parse::DefaultParser;
///
/// let source = "echo hello world";
/// let lexer = Lexer::new(source.chars());
/// let mut parser = DefaultParser::new(lexer);
/// assert!(parser.complete_command().unwrap().is_some());
/// ```
///
/// # Token lexing
///
/// Lexer implementations are free to yield tokens in whatever manner they wish,
/// however, there are a few considerations the lexer should take.
///
/// First, the lexer should consolidate consecutive tokens such as `Token::Name`,
/// `Token::Literal`, and `Token::Whitespace` as densely as possible, e.g.
/// `Literal(foobar)` is preferred over `[Literal(foo), Literal(bar)]`. Although
/// such splitting of tokens will not cause problems while parsing most shell
/// commands, certain situations require the parser to look-ahead some fixed
/// number of tokens so it can avoid backtracking. When the tokens are consolidated
/// the parser can look-ahead deterministically. If a lexer implementation chooses
/// not to use this strategy, the parser may unsuccessfully parse certain inputs
/// normally considered valid.
///
/// Second, the lexer can influence how token escaping is handled by the parser.
/// The backslash token, `\` is used to escape, or make literal, any token which
/// may or may not have a special meaning. Since the parser operates on tokens and
/// not characters, the escaping of multi-character tokens is affected by how the
/// lexer yields them. For example, the source `\<<` is normally considered by shells
/// as `[Literal(<), Less]`. If this behavior is desired, the lexer should yield
/// the tokens `[Backslash, Less, Less]` for that source. Otherwise if the lexer
/// yields the tokens `[Backslash, DLess]`, the parser will treat the source as if
/// it were `[Literal(<<)]`. The lexer's behavior need not be consistent between different
/// multi-char tokens, as long as it is aware of the implications.
#[derive(Debug)]
pub struct AutoconfParser<I, B> {
    iter: TokenIterWrapper<I>,
    builder: B,
    quotes: (Token, Token),
    quote_stack: Vec<QuoteContext>,
    last_quote_pos: Option<SourcePos>,
    detect_user_macro: bool,
}

#[derive(Debug, PartialEq, Eq)]
enum QuoteContextKind {
    Root,
    Macro(String),
    Other(String),
}

#[derive(Debug)]
struct QuoteContext {
    kind: QuoteContextKind,
    quote_level: usize,
}

impl<I: Iterator<Item = Token>, B: ShellBuilder + M4Builder + Default> AutoconfParser<I, B>
where
    B::WordFragment: Into<Option<String>> + Clone,
{
    /// Creates a new Parser from a Token iterator or collection.
    pub fn new<T>(iter: T) -> AutoconfParser<I, B>
    where
        T: IntoIterator<Item = Token, IntoIter = I>,
    {
        AutoconfParser::with_builder(iter.into_iter(), Default::default(), false)
    }

    /// Creates a new Parser with options
    pub fn new_with_config<T>(iter: T, detect_macro: bool) -> AutoconfParser<I, B>
    where
        T: IntoIterator<Item = Token, IntoIter = I>,
    {
        AutoconfParser::with_builder(iter.into_iter(), Default::default(), detect_macro)
    }
}

impl<I, L> AutoconfParser<I, builder::NodeBuilder<L>>
where
    I: Iterator<Item = Token>,
    L: Into<String> + From<String> + Clone + fmt::Debug,
    <builder::NodeBuilder<L> as builder::BuilderBase>::WordFragment:
        Into<Option<String>> + Clone,
{
    /// Parse all complete commands
    /// (special method for NodeBuilder to easily take its state)
    pub fn parse_all(mut self) -> (slab::Slab<Node<L>>, Vec<NodeId>) {
        let mut top_ids = Vec::new();
        // Parse all complete commands
        let mut take = || match self.complete_command() {
            Ok(Some(c)) => Some(Ok(c)),
            Ok(None) => None,
            Err(e) => Some(Err(e)),
        };
        while let Some(id) = take() {
            match id {
                Ok(id) => top_ids.push(id),
                Err(e) => {
                    println!("{}", e);
                    panic!();
                }
            }
        }
        (self.builder.nodes, top_ids)
    }
}

/// A macro that will consume and return a token that matches a specified pattern
/// from a parser's token iterator. If no matching token is found, None will be yielded.
macro_rules! eat_maybe {
    ($parser:expr, { $($tok:pat => $blk:block),+; _ => $default:block, }) => {
        eat_maybe!($parser, { $($tok => $blk),+; _ => $default })
    };

    ($parser:expr, { $($tok:pat => $blk:block),+,}) => {
        eat_maybe!($parser, { $($tok => $blk),+ })
    };

    ($parser:expr, { $($tok:pat => $blk:block),+ }) => {
        eat_maybe!($parser, { $($tok => $blk),+; _ => {} })
    };

    ($parser:expr, { $($tok:pat => $blk:block),+; _ => $default:block }) => {
        $(if let Some(&$tok) = $parser.iter.peek() {
            $parser.iter.next();
            $blk
        } else)+ {
            $default
        }
    };
}

/// A macro that will consume a specified token from the parser's iterator
/// an run a corresponding action block to do something with the token,
/// or it will construct and return an appropriate Unexpected(EOF) error.
macro_rules! eat {
    ($parser:expr, { $($tok:pat => $blk:block),+, }) => { eat!($parser, {$($tok => $blk),+}) };
    ($parser:expr, {$($tok:pat => $blk:block),+}) => {
        eat_maybe!($parser, {$($tok => $blk),+; _ => { return Err($parser.make_unexpected_err()) } })
    };
}

/// A macro that defines a function for parsing binary operations in arithmetic
/// expressions.  It accepts a name for the function, a name for the subsequent
/// expression (which has a higher precedence) to sub parse on, and a number of
/// tokens which can be recognized as an operator for the binary operation and
/// the appropriate AST constructor for said token/operator. All operators within
/// the definition are considered to have identical precedence and are left-to-right
/// associative.
macro_rules! arith_parse {
    ($(#[$fn_attr:meta])* fn $fn_name:ident, $next_expr:ident, $($tok:pat => $constructor:path),+) => {
        $(#[$fn_attr])*
        #[inline]
        fn $fn_name(&mut self) -> ParseResult<DefaultArithmetic, B::Error> {
            let mut expr = self.$next_expr()?;
            loop {
                self.skip_whitespace();
                eat_maybe!(self, {
                    $($tok => {
                        let next = self.$next_expr()?;
                        expr = $constructor(Box::new(expr), Box::new(next));
                    }),+;
                    _ => { break },
                });
            }
            Ok(expr)
        }
    }
}

impl<I, B> AutoconfParser<I, B>
where
    I: Iterator<Item = Token>,
    B: ShellBuilder + M4Builder,
    B::WordFragment: Into<Option<String>> + Clone,
{
    /// Construct an `Unexpected` error using the given token. If `None` specified, the next
    /// token in the iterator will be used (or `UnexpectedEOF` if none left).
    #[inline]
    fn make_unexpected_err(&mut self) -> ParseError<B::Error> {
        let pos = self.iter.pos();
        self.iter
            .next()
            .map_or(ParseError::new(UnexpectedEOF), |t| {
                ParseError::new(Unexpected(t, pos))
            })
    }

    /// Creates a new Parser from a Token iterator and provided AST builder.
    pub fn with_builder(iter: I, builder: B, detect_user_macro: bool) -> Self {
        AutoconfParser {
            iter: TokenIterWrapper::Regular(TokenIter::new(iter)),
            builder,
            quotes: (SquareOpen, SquareClose),
            quote_stack: vec![QuoteContext {
                kind: QuoteContextKind::Root,
                quote_level: 0,
            }],
            last_quote_pos: None,
            detect_user_macro,
        }
    }

    /// Returns the parser's current position in the source.
    pub fn pos(&self) -> SourcePos {
        self.iter.pos()
    }

    /// Parses a single complete command.
    ///
    /// For example, `foo && bar; baz` will yield two complete
    /// commands: `And(foo, bar)`, and `Simple(baz)`.
    pub fn complete_command(&mut self) -> ParseResult<Option<B::Command>, B::Error> {
        let start_pos = self.iter.pos();
        let pre_cmd_comments = self.linebreak();

        if self.iter.peek().is_some() {
            Ok(Some(self.complete_command_with_leading_comments(
                pre_cmd_comments,
                start_pos,
            )?))
        } else {
            if !pre_cmd_comments.is_empty() {
                self.builder.comments(pre_cmd_comments)?;
            }
            Ok(None)
        }
    }

    /// Parses a single complete command, but expects caller to parse any leading comments.
    ///
    /// It is considered an error there is not a valid complete command to be parsed, thus
    /// the caller should perform any EOF checks.
    fn complete_command_with_leading_comments(
        &mut self,
        mut pre_cmd_comments: Vec<builder::Newline>,
        start_pos: SourcePos,
    ) -> ParseResult<B::Command, B::Error> {
        if self.may_open_quote(None, false) {
            // @kui8shi
            // Since `pre_cmd_comments` do not include comments after an opening quote character,
            // We need to sweep newlines again.
            pre_cmd_comments.extend(self.linebreak());
        }
        let cmd = self.and_or_list()?;

        let (sep, cmd_comment) = eat_maybe!(self, {
            Semi => {
                self.may_close_quote(None, false);
                (builder::SeparatorKind::Semi, self.newline())
            },
            Amp  => {
                self.may_close_quote(None, false);
                (builder::SeparatorKind::Amp , self.newline())
            };
            _ => {
                self.may_close_quote(None, false);
                match self.newline() {
                    n@Some(_) => (builder::SeparatorKind::Newline, n),
                    None => (builder::SeparatorKind::Other, None),
                }
            }
        });
        let end_pos = self.iter.pos();
        Ok(self.builder.complete_command(
            pre_cmd_comments,
            cmd,
            sep,
            cmd_comment,
            (start_pos.line, end_pos.line),
        )?)
    }

    /// Parses compound AND/OR commands.
    ///
    /// Commands are left associative. For example `foo || bar && baz`
    /// parses to `And(Or(foo, bar), baz)`.
    pub fn and_or_list(&mut self) -> ParseResult<B::CommandList, B::Error> {
        let first = self.pipeline()?;
        let mut rest = Vec::new();

        loop {
            self.skip_whitespace();
            let is_and = eat_maybe!(self, {
                AndIf => { true },
                OrIf  => { false };
                _ => { break },
            });

            let post_sep_comments = self.linebreak();
            let next = self.pipeline()?;

            let next = if is_and {
                ast::AndOr::And(next)
            } else {
                ast::AndOr::Or(next)
            };

            rest.push((post_sep_comments, next));
        }

        Ok(self.builder.and_or_list(first, rest)?)
    }

    /// Parses either a single command or a pipeline of commands.
    ///
    /// For example `[!] foo | bar`.
    pub fn pipeline(&mut self) -> ParseResult<B::ListableCommand, B::Error> {
        self.skip_whitespace();
        let bang = eat_maybe!(self, {
            Bang => { true };
            _ => { false },
        });

        let mut cmds = Vec::new();
        loop {
            // We've already passed an apropriate spot for !, so it
            // is an error if it appears before the start of a command.
            if let Some(&Bang) = self.iter.peek() {
                return Err(self.make_unexpected_err());
            }

            let cmd = self.command()?;

            eat_maybe!(self, {
                Pipe => { cmds.push((self.linebreak(), cmd)) };
                _ => {
                    cmds.push((Vec::new(), cmd));
                    break;
                },
            });
        }

        Ok(self.builder.pipeline(bang, cmds)?)
    }

    /// Parses any compound or individual command.
    pub fn command(&mut self) -> ParseResult<B::PipeableCommand, B::Error> {
        if let Some(kw) = self.next_compound_command_type() {
            let compound = self.compound_command_internal(Some(kw))?;
            Ok(self.builder.compound_command_into_pipeable(compound)?)
        } else if let Some(fn_def) = self.maybe_function_declaration()? {
            Ok(fn_def)
        } else {
            self.simple_command()
        }
    }

    /// Tries to parse a simple command, e.g. `cmd arg1 arg2 >redirect`.
    ///
    /// A valid command is expected to have at least an executable name, or a single
    /// variable assignment or redirection. Otherwise an error will be returned.
    ///
    /// The format of a simple command is
    /// [<env> | <redirect>]* <executable_name> [<cmd_word> | <redirect>]*
    /// This function iterates over two loops: the first half for parsing till <executable_name>,
    /// and the second half for rest words or redirects.
    pub fn simple_command(&mut self) -> ParseResult<B::PipeableCommand, B::Error> {
        use crate::ast::{RedirectOrCmdWord, RedirectOrEnvVar};

        let (_, quote_close) = self.get_quotes();
        let mut vars = Vec::new();
        let mut cmd_args = Vec::new();

        self.may_open_quote(Some(1), true);

        // 1st loop: [EnvVar | Redirect]* + CmdWord(exec)
        loop {
            self.skip_whitespace();
            let is_name = {
                let mut peeked = self.iter.multipeek();
                if let Some(&Name(_)) = peeked.peek_next() {
                    let tok = peeked.peek_next();
                    let maybe_equal = if tok == Some(&quote_close) {
                        // var name might be quoted. (e.g. [var]=value)
                        peeked.peek_next()
                    } else {
                        tok
                    };
                    Some(&Equals) == maybe_equal
                } else {
                    false
                }
            };

            if is_name {
                if let Some(Name(var)) = self.iter.next() {
                    self.may_close_quote(None, true);
                    self.iter.next(); // Consume the =

                    let value = if let Some(&Whitespace(_)) = self.iter.peek() {
                        None
                    } else {
                        let w = match self.word_preserve_trailing_whitespace_raw_with_delim(
                            self.in_root().not().then_some(&[Comma, ParenClose]),
                            false,
                        )? {
                            Some(w) => Some(self.builder.word(w)?),
                            None => None,
                        };
                        self.skip_whitespace();
                        w
                    };
                    vars.push(RedirectOrEnvVar::EnvVar(var, value));

                    // Make sure we continue checking for assignments,
                    // otherwise it they can be interpreted as literal words.
                    continue;
                } else {
                    unreachable!();
                }
            }

            // If we find a redirect we should keep checking for
            // more redirects or assignments. Otherwise we will either
            // run into the command name or the end of the simple command.
            let exec = match self.redirect()? {
                Some(Ok(redirect)) => {
                    vars.push(RedirectOrEnvVar::Redirect(redirect));
                    continue;
                }
                Some(Err(w)) => w,
                None => break,
            };
            // Since there are no more assignments or redirects present
            // it must be the first real word, and thus the executable name.
            cmd_args.push(RedirectOrCmdWord::CmdWord(exec));
            break;
        }
        let vars = vars;

        // Now that all assignments are taken care of, any other occurances of `=` will be
        // treated as literals when we attempt to parse a word out.
        loop {
            self.may_close_quote(None, true);
            if !self.in_quote() && matches!(self.iter.peek(), Some(Comma) | Some(ParenClose)) {
                // an unquoted command macro argument ends with ',' or ')'
                break;
            }
            match self.redirect()? {
                Some(Ok(redirect)) => cmd_args.push(RedirectOrCmdWord::Redirect(redirect)),
                Some(Err(w)) => cmd_args.push(RedirectOrCmdWord::CmdWord(w)),
                None => break,
            }
        }

        // "Blank" commands are only allowed if redirection occurs
        // or if there is some variable assignment
        if vars.is_empty() && cmd_args.is_empty() {
            Err(self.make_unexpected_err())
        } else {
            Ok(self.builder.simple_command(vars, cmd_args)?)
        }
    }

    /// Parses a continuous list of redirections and will error if any words
    /// that are not valid file descriptors are found. Essentially used for
    /// parsing redirection lists after a compound command like `while` or `if`.
    pub fn redirect_list(&mut self) -> ParseResult<Vec<B::Redirect>, B::Error> {
        let mut list = Vec::new();
        loop {
            self.skip_whitespace();
            let start_pos = self.iter.pos();
            match self.redirect()? {
                Some(Ok(io)) => list.push(io),
                Some(Err(_)) => return Err(ParseError::new(BadFd(start_pos, self.iter.pos()))),
                None => break,
            }
        }

        Ok(list)
    }

    /// Parses a redirection token an any source file descriptor and
    /// path/destination descriptor as appropriate, e.g. `>out`, `1>& 2`, or `2>&-`.
    ///
    /// Since the source descriptor can be any arbitrarily complicated word,
    /// it makes it difficult to reliably peek forward whether a valid redirection
    /// exists without consuming anything. Thus this method may return a simple word
    /// if no redirection is found.
    ///
    /// Thus, unless a parse error is occured, the return value will be an optional
    /// redirect or word if either is found. In other words, `Ok(Some(Ok(redirect)))`
    /// will result if a redirect is found, `Ok(Some(Err(word)))` if a word is found,
    /// or `Ok(None)` if neither is found.
    pub fn redirect(&mut self) -> ParseResult<Option<Result<B::Redirect, B::Word>>, B::Error> {
        // fn could_be_numeric<C, W>(word: &WordKind<C, W>) -> bool {
        //     let could_be_numeric = |word: &WordKind<C, W>| match word {
        //         MayM4::Shell(w) => match w {
        //             WordKind::Star
        //             | WordKind::Question
        //             | WordKind::SquareOpen
        //             | WordKind::SquareClose
        //             | WordKind::Tilde
        //             | WordKind::Colon => false,

        //             // Literals and can be statically checked if they have non-numeric characters
        //             WordKind::Escaped(ref s) | WordKind::Literal(ref s) => {
        //                 s.chars().all(|c| c.is_ascii_digit())
        //             }

        //             // These could end up evaluating to a numeric,
        //             // but we'll have to see at runtime.
        //             WordKind::Param(_) | WordKind::Subst(_) | WordKind::CommandSubst(_) => true,
        //         },
        //         // @kui8shi
        //         // The only macro that I know to be a file descriptor, is AC_FD_CC.
        //         // It's too specific but good for remembering it in the code.
        //         M4WordKind::Macro(ref name, _) => name != "AC_FD_CC",
        //     };

        //     match *word {
        //         Simple(ref s) => could_be_numeric(s),
        //         SingleQuoted(ref s) => s.chars().all(|c| c.is_ascii_digit()),
        //         DoubleQuoted(ref fragments) => fragments.iter().all(could_be_numeric),
        //     }
        // }

        fn as_num<W>(word: &ConcatWordKind<W>) -> Option<u16>
        where
            W: Into<Option<String>> + Clone,
        {
            match word {
                Single(Simple(frag)) => {
                    if let Some(s) = frag.clone().into() {
                        u16::from_str_radix(&s, 10).ok()
                    } else {
                        None
                    }
                }
                Single(_) => None,
                Concat(ref fragments) => {
                    let mut buf = String::new();
                    for w in fragments {
                        if let Simple(frag) = w {
                            if let Some(s) = frag.clone().into() {
                                buf.push_str(&s);
                            }
                        } else {
                            return None;
                        }
                    }

                    u16::from_str_radix(&buf, 10).ok()
                }
            }
        }

        if !self.in_quote() && self.iter.peek() == Some(&Comma) {
            // @kui8shi
            // Comma can be the end of macro argument of command type.
            return Ok(None);
        }

        let (src_fd, src_fd_as_word) = match self.word_preserve_trailing_whitespace_raw_with_delim(
            self.in_root().not().then_some(&[Comma, ParenClose]),
            false,
        )? {
            None => (None, None),
            Some(w) => match as_num(&w) {
                Some(num) => (Some(num), Some(w)),
                None => return Ok(Some(Err(self.builder.word(w)?))),
            },
        };

        let redir_tok = match self.iter.peek() {
            Some(&Less) | Some(&Great) | Some(&DGreat) | Some(&Clobber) | Some(&LessAnd)
            | Some(&GreatAnd) | Some(&LessGreat) => self.iter.next().unwrap(),

            Some(&DLess) | Some(&DLessDash) => return Ok(Some(Ok(self.redirect_heredoc(src_fd)?))),

            _ => match src_fd_as_word {
                Some(w) => return Ok(Some(Err(self.builder.word(w)?))),
                None => return Ok(None),
            },
        };

        self.skip_whitespace();

        macro_rules! get_path {
            ($parser:expr) => {
                match $parser.word_preserve_trailing_whitespace_raw_with_delim(
                    $parser.in_root().not().then_some(&[Comma, ParenClose]),
                    false,
                )? {
                    Some(p) => $parser.builder.word(p)?,
                    None => return Err(self.make_unexpected_err()),
                }
            };
        }

        macro_rules! get_dup_path {
            ($parser:expr) => {{
                let path = if $parser.peek_reserved_token(&[Dash]).is_some() {
                    let dash = $parser.reserved_token(&[Dash])?;
                    Single(Simple(
                        self.builder
                            .word_fragment(WordKind::Literal(dash.to_string()))?,
                    ))
                } else {
                    // let path_start_pos = $parser.iter.pos();
                    let path = if let Some(p) = $parser
                        .word_preserve_trailing_whitespace_raw_with_delim(
                            $parser.in_root().not().then_some(&[Comma, ParenClose]),
                            false,
                        )? {
                        p
                    } else {
                        return Err($parser.make_unexpected_err());
                    };
                    path
                    // let is_numeric = match path {
                    //     Single(ref p) => could_be_numeric(&p),
                    //     Concat(ref v) => v.iter().all(could_be_numeric),
                    // };
                    // if is_numeric {
                    //     path
                    // } else {
                    //     return Err(ParseError::new(BadFd(path_start_pos, self.iter.pos())));
                    // }
                };
                $parser.builder.word(path)?
            }};
        }

        let redirect = match redir_tok {
            Less => builder::RedirectKind::Read(src_fd, get_path!(self)),
            Great => builder::RedirectKind::Write(src_fd, get_path!(self)),
            DGreat => builder::RedirectKind::Append(src_fd, get_path!(self)),
            Clobber => builder::RedirectKind::Clobber(src_fd, get_path!(self)),
            LessGreat => builder::RedirectKind::ReadWrite(src_fd, get_path!(self)),

            LessAnd => builder::RedirectKind::DupRead(src_fd, get_dup_path!(self)),
            GreatAnd => builder::RedirectKind::DupWrite(src_fd, get_dup_path!(self)),

            _ => unreachable!(),
        };

        Ok(Some(Ok(self.builder.redirect(redirect)?)))
    }

    /// Get current quoting tokens.
    /// The quoting tokens are special to programs using m4.
    /// For example, '[' and ']' are the quoting tokens in autoconf language.
    fn get_quotes(&self) -> (Token, Token) {
        self.quotes.clone()
    }

    fn is_opening_quote(&mut self) -> bool {
        let (quote_open, _) = self.get_quotes();
        self.iter.peek() == Some(&quote_open)
    }

    fn is_closing_quote(&mut self) -> bool {
        let (_, quote_close) = self.get_quotes();
        self.iter.peek() == Some(&quote_close)
    }

    /// Consume opening quote if exists.
    ///
    /// This implementation is sound for multiple calls without token consumptions.
    /// as it tracks the position of the quote token to avoid processing it multiple times.
    fn may_open_quote(&mut self, upper_limit: Option<usize>, consume: bool) -> bool {
        if upper_limit.is_some_and(|lim| lim <= *self.quote_level()) {
            // quote level reaches the upper limit.
            return false;
        }

        if !self.is_opening_quote() {
            // Not at opening quote
            return false;
        }

        // Check if we're at the same position as the last quote we've seen
        let current_pos = self.pos();
        if let Some(last_pos) = self.last_quote_pos {
            if current_pos == last_pos {
                // We've already saw this token.
                return false;
            }
        }

        // Record that we've seen this quote
        self.last_quote_pos = Some(current_pos);

        // Always adjust the level for a fresh token
        self.quote_level().add_assign(1);

        // Only consume the token if it's an outermost quote or being forced.
        let consume = consume || *self.quote_level() == 1;
        if consume {
            self.iter.next();
            // Clear the position since we consumed the token
            self.last_quote_pos = None;
        }

        consume
    }

    /// Consume closing quote token if in quotes.
    ///
    /// This implementation is sound for multiple calls without token consumptions.
    /// as it tracks the position of the quote token to avoid processing it multiple times.
    fn may_close_quote(&mut self, lower_limit: Option<usize>, consume: bool) -> bool {
        if lower_limit.is_some_and(|lim| *self.quote_level() <= lim) {
            // quote level reaches the lower limit.
            return false;
        }

        if !self.is_closing_quote() {
            // Not at closing quote
            return false;
        }

        // Check if we're at the same position as the last quote we've seen
        let current_pos = self.pos();
        if let Some(last_pos) = self.last_quote_pos {
            if current_pos == last_pos {
                // We've already saw this token.
                return false;
            }
        }

        // Record that we've seen this quote
        self.last_quote_pos = Some(current_pos);

        // Only consume the token if it's an outermost quote or being forced.
        let consume = consume || *self.quote_level() == 1;
        if consume {
            self.iter.next();
            // Clear the position since we consumed the token
            self.last_quote_pos = None;
        }

        // Adjust the level if no underflow.
        if self.in_quote() {
            self.quote_level().sub_assign(1);
        }

        consume
    }

    fn in_quote(&self) -> bool {
        self.quote_stack.last().unwrap().quote_level > 0
    }

    fn in_root(&self) -> bool {
        self.quote_stack
            .iter()
            .rev()
            .skip_while(|ctx| matches!(ctx.kind, QuoteContextKind::Other(_)))
            .next()
            .is_some_and(|ctx| matches!(ctx.kind, QuoteContextKind::Root))
    }

    fn stash_quote_context(&mut self, kind: QuoteContextKind) {
        self.quote_stack.push(QuoteContext {
            kind,
            quote_level: 0,
        })
    }

    fn pop_quote_context(&mut self) -> QuoteContext {
        let res = self.quote_stack.pop().unwrap();
        debug_assert_eq!(res.quote_level, 0);
        res
    }

    /// check function for debugging
    pub fn assert_quote_closed(&mut self) -> Result<(), ParseError<B::Error>> {
        if self.quote_stack.len() != 1 {
            return Err(self.make_unexpected_err());
        }
        assert_eq!(self.quote_stack.len(), 1);
        let res = self.quote_stack.last().unwrap();
        if res.quote_level != 0 {
            Err(self.make_unexpected_err())
        } else {
            Ok(())
        }
    }

    fn quote_level(&mut self) -> &mut usize {
        &mut self.quote_stack.last_mut().unwrap().quote_level
    }

    fn should_skip_macro_evaluation(&self) -> bool {
        use QuoteContextKind::*;
        // we skip macro evaluationwhen in quotes.
        // in the actual configure.ac, such macro calls quoted are not evaluated.
        let quote_context = self.quote_stack.last().unwrap();
        match &quote_context.kind {
            Root | Other(_) => quote_context.quote_level >= 1,
            Macro(_) => quote_context.quote_level >= 2,
        }
    }

    /// Parses a heredoc redirection and the heredoc's body.
    ///
    /// This method will look ahead after the next unquoted/unescaped newline
    /// to capture the heredoc's body, and will stop consuming tokens until
    /// the approrpiate delimeter is found on a line by itself. If the
    /// delimeter is unquoted, the heredoc's body will be expanded for
    /// parameters and other special words. Otherwise, there heredoc's body
    /// will be treated as a literal.
    ///
    /// The heredoc delimeter need not be a valid word (e.g. parameter subsitution
    /// rules within ${ } need not apply), although it is expected to be balanced
    /// like a regular word. In other words, all single/double quotes, backticks,
    /// `${ }`, `$( )`, and `( )` must be balanced.
    ///
    /// Note: if the delimeter is quoted, this method will look for an UNQUOTED
    /// version in the body. For example `<<"EOF"` will cause the parser to look
    /// until `\nEOF` is found. This it is possible to create a heredoc that can
    /// only be delimited by the end of the stream, e.g. if a newline is embedded
    /// within the delimeter. Any backticks that appear in the delimeter are
    /// expected to appear at the end of the delimeter of the heredoc body, as
    /// well as any embedded backslashes (unless the backslashes are followed by
    /// a \, $, or `).
    ///
    /// Note: this method expects that the caller provide a potential file
    /// descriptor for redirection.
    pub fn redirect_heredoc(&mut self, src_fd: Option<u16>) -> ParseResult<B::Redirect, B::Error> {
        use std::iter::FromIterator;

        macro_rules! try_map {
            ($result:expr) => {
                $result.map_err(|e: iter::UnmatchedError| ParseError::new(Unmatched(e.0, e.1)))?
            };
        }

        let strip_tabs = eat!(self, {
            DLess => { false },
            DLessDash => { true },
        });

        self.skip_whitespace();

        // Unfortunately we're going to have to capture the delimeter word "manually"
        // here and double some code. The problem is that we might need to unquote the
        // word--something that the parser isn't normally aware as a concept. We can
        // crawl a parsed WordKind tree, but we would still have to convert the inner
        // trees as either a token collection or into a string, each of which is more
        // trouble than its worth (especially since WordKind can hold a parsed and
        // and assembled Builder::Command object, which may not even be possible to
        // be represented as a string).
        //
        // Also some shells like bash and zsh seem to check for balanced tokens like
        // ', ", or ` within the heredoc delimiter (though this may just be from them
        // parsing out a word as usual), so to maintain reasonable expectations, we'll
        // do the same here.
        let mut delim_tokens = Vec::new();
        loop {
            // Normally parens are never part of words, but many
            // shells permit them to be part of a heredoc delimeter.
            if let Some(t) = self.iter.peek() {
                if t.is_word_delimiter() && t != &ParenOpen {
                    break;
                }
            } else {
                break;
            }

            for t in self.iter.balanced() {
                delim_tokens.push(try_map!(t));
            }
        }

        let mut iter = TokenIter::new(delim_tokens.into_iter());
        let mut quoted = false;
        let mut delim = String::new();
        loop {
            let start_pos = iter.pos();
            match iter.next() {
                Some(Backslash) => {
                    quoted = true;
                    if let Some(t) = iter.next() {
                        delim.push_str(t.as_str())
                    }
                }

                Some(SingleQuote) => {
                    quoted = true;
                    for t in iter.single_quoted(start_pos) {
                        delim.push_str(try_map!(t).as_str());
                    }
                }

                Some(DoubleQuote) => {
                    quoted = true;
                    let mut iter = iter.double_quoted(start_pos);
                    while let Some(next) = iter.next() {
                        match try_map!(next) {
                            Backslash => match iter.next() {
                                Some(Ok(tok @ Dollar))
                                | Some(Ok(tok @ Backtick))
                                | Some(Ok(tok @ DoubleQuote))
                                | Some(Ok(tok @ Backslash))
                                | Some(Ok(tok @ Newline)) => delim.push_str(tok.as_str()),

                                Some(t) => {
                                    let t = try_map!(t);
                                    delim.push_str(Backslash.as_str());
                                    delim.push_str(t.as_str());
                                }

                                None => delim.push_str(Backslash.as_str()),
                            },

                            t => delim.push_str(t.as_str()),
                        }
                    }
                }

                // Having backticks in a heredoc delimeter is something the major shells all
                // disagree on. Half of them (bash included) treat the precense of backticks
                // as indicating that the delimeter is quoted (and the body isn't expanded).
                // Although the POSIX standard does not indicate backticks are a form of quoting
                // its not unreasonable for them to be seen as such a way. Moreover, the presense
                // of backticks in a heredoc delimeter isn't something that is seen often, so there
                // probably won't be many problems in using this non-portable style, so we will
                // treat their presense as an indication to NOT expand the body.
                //
                // Backslashes inside the double quotes should retain their literal meaning unless
                // followed by \, $, or `, according to the POSIX standard. bash is the only major
                // shell which does not follow this rule. Since the majority of the shells seeem to
                // follow these escaping rules (one way or another), and since the standard
                // indicates this course of action, we will adopt it as well. Again, most shell
                // script maintainers probably avoid using escaping in heredoc delimeters to avoid
                // confusing, and non-portable style so picking any approach shouldn't cause too
                // many issues that cannot be fixed in a future version or with some compatability
                // flag.
                //
                // TL;DR: balanced backticks are allowed in delimeter, they cause the body to NOT
                // be expanded, and backslashes are only removed if followed by \, $, or `.
                Some(Backtick) => {
                    quoted = true;
                    delim.push_str(Backtick.as_str());
                    for t in iter.backticked_remove_backslashes(start_pos) {
                        delim.push_str(try_map!(t).as_str());
                    }
                    delim.push_str(Backtick.as_str());
                }

                Some(t) => delim.push_str(t.as_str()),
                None => break,
            }
        }

        if delim.is_empty() {
            return Err(self.make_unexpected_err());
        }

        delim.shrink_to_fit();
        let (delim, quoted) = (delim, quoted);
        let delim_len = delim.len();
        let delim_r = String::from_iter(vec![delim.as_str(), "\r"]);
        let delim_r_len = delim_r.len();

        // Here we will fast-forward to the next newline and capture the heredoc's
        // body that comes after it. Then we'll store these tokens in a safe place
        // and continue parsing them later. Although it may seem inefficient to do
        // this instead of parsing input until all pending heredocs are resolved,
        // we would have to do even more bookkeeping to store pending and resolved
        // heredocs, especially if we want to keep the builder unaware of our
        // shenanigans (since it *could* be keeping some internal state of what
        // we feed it).
        let saved_pos = self.iter.pos();
        let mut saved_tokens = Vec::new();
        while self.iter.peek().is_some() {
            // Make sure we save all tokens until the next UNQUOTED newilne
            if let Some(&Newline) = self.iter.peek() {
                saved_tokens.push(self.iter.next().unwrap());
                break;
            }

            for t in self.iter.balanced() {
                saved_tokens.push(try_map!(t));
            }
        }

        let heredoc_start_pos = self.iter.pos();
        let mut heredoc = Vec::new();
        'heredoc: loop {
            let mut line_start_pos = self.iter.pos();
            let mut line = Vec::new();
            'line: loop {
                // self.may_open_quote(None, false);
                // self.may_close_quote(None, false);
                if strip_tabs {
                    let skip_next = if let Some(Whitespace(w)) = self.iter.peek() {
                        let stripped = w.trim_start_matches('\t');
                        let num_tabs = w.len() - stripped.len();
                        line_start_pos.advance_tabs(num_tabs);

                        if !stripped.is_empty() {
                            line.push(Whitespace(stripped.to_owned()));
                        }

                        true
                    } else {
                        false
                    };

                    if skip_next {
                        self.iter.next();
                    }
                }

                let next = self.iter.next();
                match next {
                    // If we haven't grabbed any input we must have hit EOF
                    // which should delimit the heredoc body
                    None if line.is_empty() => break 'heredoc,

                    // Otherwise, if we have a partial line captured, check
                    // whether it happens to be the delimeter, and append it
                    // to the body if it isn't
                    None | Some(Newline) => {
                        // Do a quick length check on the line. Odds are that heredoc lines
                        // will be much longer than the delimeter itself, and it could get
                        // slow to stringify each and every line (and alloc it in memory)
                        // when token length checks are available without converting to strings.
                        let mut line_len = 0;
                        for t in &line {
                            line_len += t.len();
                            if line_len > delim_r_len {
                                break;
                            }
                        }

                        // NB A delimeter like "\eof" becomes [Name(e), Name(of)], which
                        // won't compare to [Name(eof)], forcing us to do a string comparison
                        // NB We must also do a check using \r\n line endings. Although we could
                        // lex \r\n as a Newline token, doing so would complicate keeping track
                        // of positions in the source, as we could have one or two byte Newlines,
                        // or two different tokens to deal with.
                        if line_len == delim_len || line_len == delim_r_len {
                            let line_str = concat_tokens(&line);
                            if line_str == delim || line_str == delim_r {
                                break 'heredoc;
                            }
                        }

                        if next == Some(Newline) {
                            line.push(Newline);
                        }
                        break 'line;
                    }

                    Some(t) => line.push(t),
                }
            }

            heredoc.push((line, line_start_pos));
        }
        self.iter
            .buffer_tokens_to_yield_first(saved_tokens, saved_pos);

        self.stash_quote_context(QuoteContextKind::Other("heredoc".into()));

        let body = if quoted {
            let body = heredoc.into_iter().flat_map(|(t, _)| t).collect::<Vec<_>>();
            Single(Simple(
                self.builder
                    .word_fragment(WordKind::Literal(concat_tokens(&body)))?,
            ))
        } else {
            let mut tok_iter = TokenIter::with_position(empty_iter(), heredoc_start_pos);
            while let Some((line, pos)) = heredoc.pop() {
                tok_iter.buffer_tokens_to_yield_first(line, pos);
            }

            let mut tok_backup = TokenIterWrapper::Buffered(tok_iter);
            mem::swap(&mut self.iter, &mut tok_backup);
            let mut body = self.word_interpolated_raw(None, heredoc_start_pos)?;
            let _ = mem::replace(&mut self.iter, tok_backup);

            if body.len() > 1 {
                Concat(body.into_iter().map(Simple).collect())
            } else {
                let body = body.pop().unwrap();
                // _or(self.builder.word_fragment(WordKind::Literal(String::new())));
                Single(Simple(body))
            }
        };

        self.pop_quote_context();

        let word = self.builder.word(body)?;
        Ok(self
            .builder
            .redirect(builder::RedirectKind::Heredoc(src_fd, word))?)
    }

    /// Parses a whitespace delimited chunk of text, honoring space quoting rules,
    /// and skipping leading and trailing whitespace.
    ///
    /// Since there are a large number of possible tokens that constitute a word,
    /// (such as literals, paramters, variables, etc.) the caller may not know
    /// for sure whether to expect a word, thus an optional result is returned
    /// in the event that no word exists.
    ///
    /// Note that an error can still arise if partial tokens are present
    /// (e.g. malformed parameter).
    pub fn word(&mut self) -> ParseResult<Option<B::Word>, B::Error> {
        let ret = self.word_preserve_trailing_whitespace()?;
        self.skip_whitespace();
        Ok(ret)
    }

    /// Identical to `Parser::word()` but preserves trailing whitespace after the word.
    pub fn word_preserve_trailing_whitespace(&mut self) -> ParseResult<Option<B::Word>, B::Error> {
        let w = match self.word_preserve_trailing_whitespace_raw()? {
            Some(w) => Some(self.builder.word(w)?),
            None => None,
        };
        Ok(w)
    }

    /// Identical to `Parser::word_preserve_trailing_whitespace()` but does
    /// not pass the result to the AST builder.
    fn word_preserve_trailing_whitespace_raw(
        &mut self,
    ) -> ParseResult<Option<ConcatWordKind<B::WordFragment>>, B::Error> {
        self.word_preserve_trailing_whitespace_raw_with_delim(None, false)
    }

    /// Identical to `Parser::word_preserve_trailing_whitespace_raw()` but
    /// allows for specifying an arbitrary token as a word delimiter.
    fn word_preserve_trailing_whitespace_raw_with_delim(
        &mut self,
        delims: Option<&[Token]>,
        refresh_quote_context: bool,
    ) -> ParseResult<Option<ConcatWordKind<B::WordFragment>>, B::Error> {
        self.skip_whitespace();
        let (quote_open, quote_close) = self.get_quotes();

        // Make sure we don't consume comments,
        // e.g. if a # is at the start of a word.
        match self.iter.peek() {
            Some(&Pound) => return Ok(None),
            Some(Name(n)) if n == "dnl" => return Ok(None),
            _ => (),
        }

        if refresh_quote_context {
            self.stash_quote_context(QuoteContextKind::Other("refreshed word".into()));
        }

        let mut words = Vec::new();
        loop {
            let quote_level = *self.quote_level();
            if quote_level == 0 {
                if let Some(delims) = delims {
                    if let Some(tok) = self.iter.peek() {
                        if delims.iter().any(|t| t == tok) {
                            break;
                        } else if tok == &quote_close {
                            break;
                        }
                    }
                }
            }

            // check an edge case of quoted parameter
            let res = {
                let mut m = self.iter.multipeek();
                [&quote_open, &Dollar, &quote_close]
                    .iter()
                    .all(|t| m.peek_next() == Some(t))
            };
            if res {
                // it must be a parameter with dollar quoted
                // such quoting pairs should be consumed within parameter_raw
                let param = self.parameter_raw()?;
                words.push(Simple(self.builder.word_fragment(param)?));
                continue;
            }

            if self.may_open_quote(None, false) {
                continue;
            }

            if self.may_close_quote(None, false) {
                continue;
            }

            match self.iter.peek() {
                Some(&CurlyOpen) | Some(&CurlyClose) | Some(&SquareOpen) | Some(&SquareClose)
                | Some(&SingleQuote) | Some(&DoubleQuote) | Some(&Pound) | Some(&Star)
                | Some(&Question) | Some(&Tilde) | Some(&Bang) | Some(&Backslash)
                | Some(&Percent) | Some(&Dash) | Some(&Equals) | Some(&Plus) | Some(&Colon)
                | Some(&At) | Some(&Caret) | Some(&Slash) | Some(&Comma) | Some(&Dot)
                | Some(&Name(_)) | Some(&Literal(_)) => {}

                Some(&Backtick) => {
                    let backtick = self.backticked_raw()?;
                    words.push(Simple(self.builder.word_fragment(backtick)?));
                    continue;
                }

                Some(&Dollar) | Some(&ParamPositional(_)) => {
                    let param = self.parameter_raw()?;
                    words.push(Simple(self.builder.word_fragment(param)?));
                    continue;
                }

                Some(&EmptyQuotes) => {
                    // Empty quotes work like an empty string while being a delimiter of tokens.
                    self.iter.next();
                    continue;
                }

                Some(&Whitespace(_)) if quote_level > 1 => {
                    self.iter.next();
                    continue;
                }

                Some(&Newline) | Some(&ParenOpen) | Some(&ParenClose) | Some(&Semi)
                | Some(&Amp) | Some(&Pipe) | Some(&AndIf) | Some(&OrIf) | Some(&DSemi)
                | Some(&Less) | Some(&Great) | Some(&DLess) | Some(&DGreat) | Some(&GreatAnd)
                | Some(&LessAnd) | Some(&DLessDash) | Some(&Clobber) | Some(&LessGreat)
                | Some(&Whitespace(_)) | None => break,
            }

            if let Some(Name(_)) = self.iter.peek() {
                if let Some(name) = self.maybe_macro_call() {
                    let macro_call = self.macro_call(&[&name])?;
                    words.push(Simple(self.builder.macro_into_word(macro_call)?));
                    continue;
                }
            }

            let start_pos = self.iter.pos();
            let w = match self.iter.next().unwrap() {
                // Unless we are explicitly parsing a brace group, `{` and `}` should
                // be treated as literals.
                //
                // Also, comments are only recognized where a Newline is valid, thus '#'
                // becomes a literal if it occurs in the middle of a word.
                tok @ Bang
                | tok @ Pound
                | tok @ Percent
                | tok @ Dash
                | tok @ Equals
                | tok @ Plus
                | tok @ At
                | tok @ Caret
                | tok @ Slash
                | tok @ Comma
                | tok @ Dot
                | tok @ CurlyOpen
                | tok @ CurlyClose => Simple(
                    self.builder
                        .word_fragment(WordKind::Literal(tok.to_string()))?,
                ),

                Name(s) | Literal(s) => Simple(self.builder.word_fragment(WordKind::Literal(s))?),
                Star => Simple(self.builder.word_fragment(WordKind::Star)?),
                Question => Simple(self.builder.word_fragment(WordKind::Question)?),
                Tilde => Simple(self.builder.word_fragment(WordKind::Tilde)?),
                SquareOpen => Simple(self.builder.word_fragment(WordKind::SquareOpen)?),
                SquareClose => Simple(self.builder.word_fragment(WordKind::SquareClose)?),
                Colon => Simple(self.builder.word_fragment(WordKind::Colon)?),

                Backslash => match self.iter.next() {
                    // Escaped newlines become whitespace and a delimiter.
                    // Alternatively, can't escape EOF, just ignore the slash
                    Some(Newline) | None => break,
                    Some(t) => Simple(
                        self.builder
                            .word_fragment(WordKind::Escaped(t.to_string()))?,
                    ),
                },

                SingleQuote => {
                    let mut buf = String::new();
                    for t in self.iter.single_quoted(start_pos) {
                        buf.push_str(
                            t.map_err(|e| ParseError::new(Unmatched(e.0, e.1)))?
                                .as_str(),
                        )
                    }

                    SingleQuoted(buf)
                }

                DoubleQuote => {
                    let res =
                        DoubleQuoted(self.word_interpolated_raw(Some(&[DoubleQuote]), start_pos)?);
                    eat!(self, { DoubleQuote => {} });
                    res
                }

                // Parameters and backticks should have been
                // handled while peeking above.
                Backtick | Dollar | ParamPositional(_) => unreachable!(),

                // All word delimiters should have
                // broken the loop while peeking above.
                Newline | ParenOpen | ParenClose | Semi | Amp | Pipe | AndIf | OrIf | DSemi
                | Less | Great | DLess | DGreat | GreatAnd | LessAnd | DLessDash | Clobber
                | LessGreat | Whitespace(_) | EmptyQuotes => unreachable!(),
            };

            words.push(w);
        }

        if refresh_quote_context {
            self.pop_quote_context();
        }

        let ret = if words.is_empty() {
            None
        } else if words.len() == 1 {
            Some(Single(words.pop().unwrap()))
        } else {
            Some(Concat(words))
        };

        Ok(ret)
    }

    /// Parses tokens in a way similar to how double quoted strings may be interpreted.
    ///
    /// Parameters/substitutions are parsed as normal, backslashes keep their literal
    /// meaning unless they preceed $, `, ", \, \n, or the specified delimeter, while
    /// all other tokens are consumed as literals.
    ///
    /// Tokens will continue to be consumed until a specified delimeter is reached
    /// (which is also consumed). If EOF is reached before a delimeter can be found,
    /// an error will result. If a `None` is provided as a delimeter tokens will be
    /// consumed until EOF is reached and no error will result.
    ///
    /// `delim` argument structure is Option<(&[close tokens], consume)>. The close
    /// token indicates when to stop parsing the word. If None, the parsing stops at EOF.
    fn word_interpolated_raw(
        &mut self,
        delims: Option<&[Token]>,
        start_pos: SourcePos,
    ) -> ParseResult<Vec<B::WordFragment>, B::Error> {
        let mut words = Vec::new();
        let mut buf = String::new();
        let (quote_open, quote_close) = self.get_quotes();
        let is_delim = |tok: Option<Token>| {
            if let Some(delims) = delims {
                delims.iter().find(|&d| Some(d) == tok.as_ref()).is_some()
            } else {
                tok.is_none()
            }
        };
        let delim_is_quote = is_delim(Some(quote_close.clone()));
        loop {
            if delim_is_quote {
                if !self.in_quote() {
                    break;
                }
            } else if is_delim(self.iter.peek().cloned()) {
                break;
            }

            macro_rules! store {
                ($word:expr) => {{
                    if !buf.is_empty() {
                        words.push(self.builder.word_fragment(WordKind::Literal(buf))?);
                        buf = String::new();
                    }
                    words.push($word);
                }};
            }

            // check macro call
            if let Some(Name(_)) = self.iter.peek() {
                if let Some(name) = self.maybe_macro_call() {
                    let macro_call = self.macro_call(&[&name])?;
                    store!(self.builder.macro_into_word(macro_call)?);
                    continue;
                }
            }

            // check an edge case of quoted parameter
            let res = {
                let mut m = self.iter.multipeek();
                [&quote_open, &Dollar, &quote_close]
                    .iter()
                    .all(|t| m.peek_next() == Some(t))
            };
            if res {
                // it must be a parameter with dollar quoted
                // such quoting pairs should be consumed within parameter_raw
                let param = self.parameter_raw()?;
                store!(self.builder.word_fragment(param)?);
                continue;
            }

            if self.may_open_quote(None, false) || self.may_close_quote(None, false) {
                continue;
            }

            // Make sure we don't consume any $ (or any specific parameter token)
            // we find since the `parameter` method expects to consume them.
            match self.iter.peek() {
                Some(&Dollar) | Some(&ParamPositional(_)) => {
                    let param = self.parameter_raw()?;
                    store!(self.builder.word_fragment(param)?);
                    continue;
                }

                Some(&Backtick) => {
                    let backtick = self.backticked_raw()?;
                    store!(self.builder.word_fragment(backtick)?);
                    continue;
                }

                _ => {}
            }

            match self.iter.next() {
                // Backslashes only escape a few tokens when double-quoted-type words
                Some(Backslash) => {
                    let special = match self.iter.peek() {
                        Some(&Dollar) | Some(&Backtick) | Some(&DoubleQuote) | Some(&Backslash)
                        | Some(&Newline) => true,
                        _ => false,
                    };

                    if special || is_delim(self.iter.peek().cloned()) {
                        store!(self.builder.word_fragment(WordKind::Escaped(
                            self.iter.next().unwrap().to_string()
                        ))?)
                    } else {
                        buf.push_str(Backslash.as_str());
                    }
                }

                Some(Dollar) => unreachable!(),   // Sanity
                Some(Backtick) => unreachable!(), // Sanity

                Some(EmptyQuotes) if !self.in_quote() => {
                    // even in interpolated shell strings, raw empty quotes should be ignored.
                }

                Some(t) => buf.push_str(t.as_str()),
                None => match delims {
                    Some(delims) => {
                        return Err(ParseError::new(Unmatched(
                            delims.last().unwrap().clone(),
                            start_pos,
                        )))
                    }
                    None => break,
                },
            }
        }

        if !buf.is_empty() {
            words.push(self.builder.word_fragment(WordKind::Literal(buf))?);
        }

        Ok(words)
    }

    /// Parses a command subsitution in the form \`cmd\`.
    ///
    /// Any backslashes that are immediately followed by \, $, or ` are removed
    /// before the contents inside the original backticks are recursively parsed
    /// as a command.
    pub fn backticked_command_substitution(&mut self) -> ParseResult<B::Word, B::Error> {
        let word = self.backticked_raw()?;
        let word_fragment = self.builder.word_fragment(word)?;
        Ok(self.builder.word(Single(Simple(word_fragment)))?)
    }

    /// Identical to `Parser::backticked_command_substitution`, except but does not pass the
    /// result to the AST builder.
    fn backticked_raw(&mut self) -> ParseResult<WordKind<B::Command, B::WordFragment>, B::Error> {
        let backtick_pos = self.iter.pos();
        eat!(self, { Backtick => {} });

        // FIXME: it would be great to not have to buffer all tokens between backticks
        // and lazily consume them. Unfortunately the BacktickBackslashRemover iterator
        // returns a Result<Token, UnmatchedError>, so we can't temporarily substitute it
        // for our regular iterator (without forcing us to check the the value of each
        // `peek` or `next` operation we make).
        let tok_iter = self
            .iter
            .token_iter_from_backticked_with_removed_backslashes(backtick_pos)
            .map_err(|e| ParseError::new(Unmatched(e.0, e.1)))?;

        let mut tok_backup = TokenIterWrapper::Buffered(tok_iter);

        mem::swap(&mut self.iter, &mut tok_backup);
        let cmd_subst = self.command_group_internal(CommandGroupDelimiters::default());
        let _ = mem::replace(&mut self.iter, tok_backup);

        Ok(WordKind::CommandSubst(cmd_subst?))
    }

    /// Parses a parameters such as `$$`, `$1`, `$foo`, etc, or
    /// parameter substitutions such as `$(cmd)`, `${param-word}`, etc.
    ///
    /// Since it is possible that a leading `$` is not followed by a valid
    /// parameter, the `$` should be treated as a literal. Thus this method
    /// returns an `Word`, which will capture both cases where a literal or
    /// parameter is parsed.
    pub fn parameter(&mut self) -> ParseResult<B::Word, B::Error> {
        let param = self.parameter_raw()?;
        let word_fragment = self.builder.word_fragment(param)?;
        Ok(self.builder.word(Single(Simple(word_fragment)))?)
    }

    /// Identical to `Parser::parameter()` but does not pass the result to the AST builder.
    fn parameter_raw(&mut self) -> ParseResult<WordKind<B::Command, B::WordFragment>, B::Error> {
        use crate::ast::Parameter;

        let start_pos = self.iter.pos();
        self.stash_quote_context(QuoteContextKind::Other("parameter".into()));
        self.may_open_quote(None, false);
        let ret = match self.iter.next() {
            Some(ParamPositional(p)) => Ok(WordKind::Param(Parameter::Positional(p as u32))),

            Some(Dollar) => {
                // dollar might be quoted (e.g. [$]var)
                self.may_close_quote(None, false);
                // an empty quote might be inserted (e.g. $[]var)
                eat_maybe!(self, { EmptyQuotes => {} });
                // parameter body might be quoted (e.g. $[var])
                let in_quote = self.may_open_quote(None, false);
                let p = match self.iter.peek() {
                    Some(&Star) | Some(&Pound) | Some(&Question) | Some(&Dollar) | Some(&Bang)
                    | Some(&Dash) | Some(&At) | Some(&Name(_)) => {
                        Ok(WordKind::Param(self.parameter_inner()?))
                    }

                    Some(&ParenOpen) | Some(&CurlyOpen) => self.parameter_substitution_raw(),

                    Some(Literal(s)) if s.len() == 1 && s.chars().last().unwrap().is_numeric() => {
                        // quoted positional parameter (e.g. $[1])
                        let num = s.parse::<u32>().unwrap();
                        self.iter.next();
                        Ok(WordKind::Param(Parameter::Positional(num)))
                    }

                    _ => Ok(WordKind::Literal(Dollar.to_string())),
                };
                if in_quote {
                    self.may_close_quote(None, false);
                }
                p
            }

            Some(t) => Err(ParseError::new(Unexpected(t, start_pos))),
            None => Err(ParseError::new(UnexpectedEOF)),
        };
        self.pop_quote_context();
        ret
    }

    /// Parses the word part of a parameter substitution, up to and including
    /// the closing curly brace.
    ///
    /// All tokens that normally cannot be part of a word will be treated
    /// as literals.
    fn parameter_substitution_word_raw(
        &mut self,
        curly_open_pos: SourcePos,
    ) -> ParseResult<Option<ConcatWordKind<B::WordFragment>>, B::Error> {
        let mut words = Vec::new();
        'capture_words: loop {
            'capture_literals: loop {
                let found_backslash = match self.iter.peek() {
                    None | Some(&CurlyClose) => break 'capture_words,

                    Some(&Backslash) => true,

                    // Tokens that are normally skipped or ignored either always or at the
                    // start of a word (e.g. #), we want to make sure we capture these ourselves.
                    Some(t @ &Pound)
                    | Some(t @ &ParenOpen)
                    | Some(t @ &ParenClose)
                    | Some(t @ &Semi)
                    | Some(t @ &Amp)
                    | Some(t @ &Pipe)
                    | Some(t @ &AndIf)
                    | Some(t @ &OrIf)
                    | Some(t @ &DSemi)
                    | Some(t @ &Less)
                    | Some(t @ &Great)
                    | Some(t @ &DLess)
                    | Some(t @ &DGreat)
                    | Some(t @ &GreatAnd)
                    | Some(t @ &LessAnd)
                    | Some(t @ &DLessDash)
                    | Some(t @ &Clobber)
                    | Some(t @ &LessGreat)
                    | Some(t @ &EmptyQuotes)
                    | Some(t @ &Whitespace(_))
                    | Some(t @ &Newline) => {
                        words.push(Simple(
                            self.builder
                                .word_fragment(WordKind::Literal(t.as_str().to_owned()))?,
                        ));
                        false
                    }

                    // Tokens that are always part of a word,
                    // don't have to handle them differently.
                    Some(&CurlyOpen)
                    | Some(&SquareOpen)
                    | Some(&SquareClose)
                    | Some(&SingleQuote)
                    | Some(&DoubleQuote)
                    | Some(&Star)
                    | Some(&Question)
                    | Some(&Tilde)
                    | Some(&Bang)
                    | Some(&Percent)
                    | Some(&Dash)
                    | Some(&Equals)
                    | Some(&Plus)
                    | Some(&Colon)
                    | Some(&At)
                    | Some(&Caret)
                    | Some(&Slash)
                    | Some(&Comma)
                    | Some(&Dot)
                    | Some(&Name(_))
                    | Some(&Literal(_))
                    | Some(&Backtick)
                    | Some(&Dollar)
                    | Some(&ParamPositional(_)) => break 'capture_literals,
                };

                // Escaped newlines are considered whitespace and usually ignored,
                // so we have to manually capture here.
                let skip_twice = if found_backslash {
                    let mut peek = self.iter.multipeek();
                    peek.peek_next(); // Skip past the Backslash
                    if let Some(t @ &Newline) = peek.peek_next() {
                        words.push(Simple(
                            self.builder
                                .word_fragment(WordKind::Escaped(t.as_str().to_owned()))?,
                        ));
                        true
                    } else {
                        // Backslash is escaping something other than newline,
                        // capture it like a regular word.
                        break 'capture_literals;
                    }
                } else {
                    false
                };

                self.iter.next(); // Skip the first token that was peeked above
                if skip_twice {
                    self.iter.next();
                }
            }

            match self
                .word_preserve_trailing_whitespace_raw_with_delim(Some(&[CurlyClose]), true)?
            {
                Some(Single(w)) => words.push(w),
                Some(Concat(ws)) => words.extend(ws),
                None => break 'capture_words,
            }
        }

        eat_maybe!(self, {
            CurlyClose => {};
            _ => { return Err(ParseError::new(Unmatched(CurlyOpen, curly_open_pos))); }
        });

        if words.is_empty() {
            Ok(None)
        } else if words.len() == 1 {
            Ok(Some(Single(words.pop().unwrap())))
        } else {
            Ok(Some(Concat(words)))
        }
    }

    /// Parses everything that comes after the parameter in a parameter substitution.
    ///
    /// For example, given `${<param><colon><operator><word>}`, this function will consume
    /// the colon, operator, word, and closing curly.
    ///
    /// Nothing is passed to the builder.
    fn parameter_substitution_body_raw(
        &mut self,
        param: DefaultParameter,
        curly_open_pos: SourcePos,
    ) -> ParseResult<WordKind<B::Command, B::WordFragment>, B::Error> {
        use crate::ast::builder::ParameterSubstitutionKind::*;
        use crate::ast::Parameter;

        let has_colon = eat_maybe!(self, {
            Colon => { true };
            _ => { false },
        });

        let op_pos = self.iter.pos();
        let op = match self.iter.next() {
            Some(tok @ Dash) | Some(tok @ Equals) | Some(tok @ Question) | Some(tok @ Plus) => tok,

            Some(CurlyClose) => return Ok(WordKind::Param(param)),

            Some(t) => return Err(ParseError::new(BadSubst(t, op_pos))),
            None => return Err(ParseError::new(Unmatched(CurlyOpen, curly_open_pos))),
        };

        let word = self.parameter_substitution_word_raw(curly_open_pos)?;
        let maybe_len = param == Parameter::Pound && !has_colon && word.is_none();

        // We must carefully check if we get ${#-} or ${#?}, in which case
        // we have parsed a Len substitution and not something else
        let ret = if maybe_len && op == Dash {
            Len(Parameter::Dash)
        } else if maybe_len && op == Question {
            Len(Parameter::Question)
        } else {
            match op {
                Dash => Default(has_colon, param, word),
                Equals => Assign(has_colon, param, word),
                Question => Error(has_colon, param, word),
                Plus => Alternative(has_colon, param, word),
                _ => unreachable!(),
            }
        };
        Ok(WordKind::Subst(Box::new(ret)))
    }

    /// Parses a parameter substitution in the form of `${...}`, `$(...)`, or `$((...))`.
    /// Nothing is passed to the builder.
    fn parameter_substitution_raw(
        &mut self,
    ) -> ParseResult<WordKind<B::Command, B::WordFragment>, B::Error> {
        use crate::ast::builder::ParameterSubstitutionKind::*;
        use crate::ast::Parameter;

        let start_pos = self.iter.pos();
        match self.iter.peek() {
            Some(&ParenOpen) => {
                let is_arith = {
                    let mut peeked = self.iter.multipeek();
                    peeked.peek_next(); // Skip first ParenOpen
                    Some(&ParenOpen) == peeked.peek_next()
                };

                let subst = if is_arith {
                    eat!(self, { ParenOpen => {} });
                    eat!(self, { ParenOpen => {} });

                    // If we hit a paren right off the bat either the body is empty
                    // or there is a stray paren which will result in an error either
                    // when we look for the closing parens or sometime after.
                    self.skip_whitespace();
                    let subst = if let Some(&ParenClose) = self.iter.peek() {
                        None
                    } else {
                        Some(self.arithmetic_substitution()?)
                    };

                    // Some shells allow the closing parens to have whitespace in between
                    self.skip_whitespace();
                    eat!(self, { ParenClose => {} });
                    self.skip_whitespace();
                    eat!(self, { ParenClose => {} });

                    Arith(subst)
                } else {
                    Command(self.subshell_internal(true)?)
                };

                Ok(WordKind::Subst(Box::new(subst)))
            }

            Some(&CurlyOpen) => {
                let curly_open_pos = start_pos;
                self.iter.next();

                let param = self.parameter_inner()?;
                let subst = match self.iter.peek() {
                    Some(&Percent) => {
                        self.iter.next();
                        eat_maybe!(self, {
                            Percent => {
                                let word = self.parameter_substitution_word_raw(curly_open_pos);
                                RemoveLargestSuffix(param, word?)
                            };
                            _ => {
                                let word = self.parameter_substitution_word_raw(curly_open_pos);
                                RemoveSmallestSuffix(param, word?)
                            }
                        })
                    }

                    Some(&Pound) => {
                        self.iter.next();
                        eat_maybe!(self, {
                            Pound => {
                                let word = self.parameter_substitution_word_raw(curly_open_pos);
                                RemoveLargestPrefix(param, word?)
                            };
                            _ => {
                                match self.parameter_substitution_word_raw(curly_open_pos)? {
                                    // Handle ${##} case
                                    None if Parameter::Pound == param => Len(Parameter::Pound),
                                    w => RemoveSmallestPrefix(param, w),
                                }
                            }
                        })
                    }

                    // In this case the found # is the parameter itself
                    Some(&Colon) | Some(&Dash) | Some(&Equals) | Some(&Question) | Some(&Plus)
                    | Some(&CurlyClose)
                        if Parameter::Pound == param =>
                    {
                        return self.parameter_substitution_body_raw(param, curly_open_pos)
                    }

                    // Otherwise we must have ${#param}
                    _ if Parameter::Pound == param => {
                        let param = self.parameter_inner()?;
                        eat!(self, { CurlyClose => { Len(param) } })
                    }

                    _ => return self.parameter_substitution_body_raw(param, curly_open_pos),
                };

                Ok(WordKind::Subst(Box::new(subst)))
            }

            _ => Err(self.make_unexpected_err()),
        }
    }

    /// Parses a valid parameter that can appear inside a set of curly braces.
    fn parameter_inner(&mut self) -> ParseResult<DefaultParameter, B::Error> {
        use crate::ast::Parameter;
        let start_pos = self.iter.pos();
        let param = match self.iter.next() {
            Some(Star) => Parameter::Star,
            Some(Pound) => Parameter::Pound,
            Some(Question) => Parameter::Question,
            Some(Dollar) => Parameter::Dollar,
            Some(Bang) => Parameter::Bang,
            Some(Dash) => Parameter::Dash,
            Some(At) => Parameter::At,

            Some(Name(n)) => Parameter::Var(n),
            Some(Literal(s)) => match u32::from_str(&s) {
                Ok(n) => Parameter::Positional(n),
                Err(_) => return Err(ParseError::new(BadSubst(Literal(s), start_pos))),
            },

            Some(t) => return Err(ParseError::new(BadSubst(t, start_pos))),
            None => return Err(ParseError::new(UnexpectedEOF)),
        };

        Ok(param)
    }

    /// Parses any number of sequential commands between the `do` and `done`
    /// reserved words. Each of the reserved words must be a literal token, and cannot be
    /// quoted or concatenated.
    pub fn do_group(&mut self) -> ParseResult<builder::CommandGroup<B::Command>, B::Error> {
        let start_pos = self.iter.pos();
        self.reserved_word(&[DO])
            .map_err(|_| self.make_unexpected_err())?;
        let result = self.command_group(CommandGroupDelimiters {
            reserved_words: &[DONE],
            ..Default::default()
        })?;
        self.reserved_word(&[DONE])
            .map_err(|()| ParseError::new(IncompleteCmd(DO, start_pos, DONE, self.iter.pos())))?;
        Ok(result)
    }

    /// Parses any number of sequential commands between balanced `{` and `}`
    /// reserved words. Each of the reserved words must be a literal token, and cannot be quoted.
    pub fn brace_group(&mut self) -> ParseResult<builder::CommandGroup<B::Command>, B::Error> {
        // CurlyClose must be encountered as a stand alone word,
        // even though it is represented as its own token
        let start_pos = self.iter.pos();
        self.reserved_token(&[CurlyOpen])?;
        let cmds = self.command_group(CommandGroupDelimiters {
            reserved_tokens: &[CurlyClose],
            ..Default::default()
        })?;
        self.reserved_token(&[CurlyClose])
            .map_err(|_| ParseError::new(Unmatched(CurlyOpen, start_pos)))?;
        Ok(cmds)
    }

    /// Parses any number of sequential commands between balanced `(` and `)`.
    ///
    /// It is considered an error if no commands are present inside the subshell.
    pub fn subshell(&mut self) -> ParseResult<builder::CommandGroup<B::Command>, B::Error> {
        self.subshell_internal(false)
    }

    /// Like `Parser::subshell` but allows the caller to specify
    /// if an empty body constitutes an error or not.
    fn subshell_internal(
        &mut self,
        empty_body_ok: bool,
    ) -> ParseResult<builder::CommandGroup<B::Command>, B::Error> {
        let start_pos = self.iter.pos();
        eat!(self, { ParenOpen => {} });

        // Parens are always special tokens
        let body = self.command_group_internal(CommandGroupDelimiters {
            exact_tokens: &[ParenClose],
            ..Default::default()
        })?;

        match self.iter.peek() {
            Some(&ParenClose) if empty_body_ok || !body.commands.is_empty() => {
                self.iter.next();
                Ok(body)
            }
            Some(_) => Err(self.make_unexpected_err()),
            None => Err(ParseError::new(Unmatched(ParenOpen, start_pos))),
        }
    }

    /// Peeks at the next token (after skipping whitespace) to determine
    /// if (and which) compound command may follow.
    fn next_compound_command_type(&mut self) -> Option<CompoundCmdKeyword> {
        self.skip_whitespace();
        if Some(&ParenOpen) == self.iter.peek() {
            Some(CompoundCmdKeyword::Subshell)
        } else if self.peek_reserved_token(&[CurlyOpen]).is_some() {
            Some(CompoundCmdKeyword::Brace)
        } else if let Some(name) = self.maybe_macro_call() {
            Some(CompoundCmdKeyword::Macro(name))
        } else {
            match self.peek_reserved_word(&[FOR, CASE, IF, WHILE, UNTIL]) {
                Some(FOR) => Some(CompoundCmdKeyword::For),
                Some(CASE) => Some(CompoundCmdKeyword::Case),
                Some(IF) => Some(CompoundCmdKeyword::If),
                Some(WHILE) => Some(CompoundCmdKeyword::While),
                Some(UNTIL) => Some(CompoundCmdKeyword::Until),
                _ => None,
            }
        }
    }

    /// Parses compound commands like `for`, `case`, `if`, `while`, `until`,
    /// brace groups, or subshells, including any redirection lists to be applied to them.
    pub fn compound_command(&mut self) -> ParseResult<B::CompoundCommand, B::Error> {
        self.compound_command_internal(None)
    }

    /// Slightly optimized version of `Parse::compound_command` that will not
    /// check an upcoming reserved word if the caller already knows the answer.
    fn compound_command_internal(
        &mut self,
        kw: Option<CompoundCmdKeyword>,
    ) -> ParseResult<B::CompoundCommand, B::Error> {
        self.may_open_quote(None, true);
        let cmd = match kw.or_else(|| self.next_compound_command_type()) {
            Some(CompoundCmdKeyword::If) => {
                let fragments = self.if_command()?;
                self.may_close_quote(None, true);
                let io = self.redirect_list()?;
                self.builder.if_command(fragments, io)?
            }

            Some(CompoundCmdKeyword::While) | Some(CompoundCmdKeyword::Until) => {
                let (until, guard_body_pair) = self.loop_command()?;
                self.may_close_quote(None, true);
                let io = self.redirect_list()?;
                self.builder.loop_command(until, guard_body_pair, io)?
            }

            Some(CompoundCmdKeyword::For) => {
                let for_fragments = self.for_command()?;
                self.may_close_quote(None, true);
                let io = self.redirect_list()?;
                self.builder.for_command(for_fragments, io)?
            }

            Some(CompoundCmdKeyword::Case) => {
                let fragments = self.case_command()?;
                self.may_close_quote(None, true);
                let io = self.redirect_list()?;
                self.builder.case_command(fragments, io)?
            }

            Some(CompoundCmdKeyword::Brace) => {
                let cmds = self.brace_group()?;
                self.may_close_quote(None, true);
                let io = self.redirect_list()?;
                self.builder.brace_group(cmds, io)?
            }

            Some(CompoundCmdKeyword::Subshell) => {
                let cmds = self.subshell()?;
                self.may_close_quote(None, true);
                let io = self.redirect_list()?;
                self.builder.subshell(cmds, io)?
            }

            Some(CompoundCmdKeyword::Macro(name)) => {
                let cmds = self.macro_call(&[&name])?;
                self.may_close_quote(None, true);

                self.skip_whitespace();
                let io = match self.iter.peek() {
                    Some(
                        Less | Great | DLess | DGreat | GreatAnd | LessAnd | DLessDash | Clobber
                        | LessGreat,
                    ) => self.redirect_list()?,
                    // @kui8shi
                    // FIXME: Macro of commands sometimes don't end with newline but with another command
                    // which will be incorrectly treated as redirection words, leading to an error.
                    // To ease the problem, we ban the redirection after macro of commands.
                    // Offcourse by this dumb fix, the parser loses its complete support for redirection
                    // following a macro of command. So we may need an alternative fix later.
                    _ => vec![],
                };

                self.builder.macro_into_compound_command(cmds, io)?
            }

            None => return Err(self.make_unexpected_err()),
        };
        self.may_close_quote(None, false);

        Ok(cmd)
    }

    /// Parses loop commands like `while` and `until` but does not parse any
    /// redirections that may follow.
    ///
    /// Since they are compound commands (and can have redirections applied to
    /// the entire loop) this method returns the relevant parts of the loop command,
    /// without constructing an AST node, it so that the caller can do so with redirections.
    pub fn loop_command(
        &mut self,
    ) -> ParseResult<(builder::LoopKind, builder::GuardBodyPairGroup<B::Command>), B::Error> {
        let start_pos = self.iter.pos();
        let kind = match self
            .reserved_word(&[WHILE, UNTIL])
            .map_err(|_| self.make_unexpected_err())?
        {
            WHILE => builder::LoopKind::While,
            UNTIL => builder::LoopKind::Until,
            _ => unreachable!(),
        };
        let guard = self.command_group(CommandGroupDelimiters {
            reserved_words: &[DO],
            ..Default::default()
        })?;
        match self.peek_reserved_word(&[DO]) {
            Some(_) => Ok((
                kind,
                builder::GuardBodyPairGroup {
                    guard,
                    body: self.do_group()?,
                },
            )),
            None => Err(ParseError::new(IncompleteCmd(
                WHILE,
                start_pos,
                DO,
                self.iter.pos(),
            ))),
        }
    }

    /// Parses a single `if` command but does not parse any redirections that may follow.
    ///
    /// Since `if` is a compound command (and can have redirections applied to it) this
    /// method returns the relevant parts of the `if` command, without constructing an
    /// AST node, it so that the caller can do so with redirections.
    pub fn if_command(&mut self) -> ParseResult<builder::IfFragments<B::Command>, B::Error> {
        let start_pos = self.iter.pos();
        self.reserved_word(&[IF])
            .map_err(|_| self.make_unexpected_err())?;

        macro_rules! missing_fi {
            () => {
                |_| ParseError::new(IncompleteCmd(IF, start_pos, FI, self.iter.pos()))
            };
        }

        macro_rules! missing_then {
            () => {
                |_| ParseError::new(IncompleteCmd(IF, start_pos, THEN, self.iter.pos()))
            };
        }

        let mut conditionals = Vec::new();
        loop {
            let guard = self.command_group(CommandGroupDelimiters {
                reserved_words: &[THEN],
                ..Default::default()
            })?;
            self.reserved_word(&[THEN]).map_err(missing_then!())?;

            let body = self.command_group(CommandGroupDelimiters {
                reserved_words: &[ELIF, ELSE, FI],
                ..Default::default()
            })?;
            conditionals.push(builder::GuardBodyPairGroup { guard, body });

            let els = match self
                .reserved_word(&[ELIF, ELSE, FI])
                .map_err(missing_fi!())?
            {
                ELIF => continue,
                ELSE => {
                    let els = self.command_group(CommandGroupDelimiters {
                        reserved_words: &[FI],
                        ..Default::default()
                    })?;
                    self.reserved_word(&[FI]).map_err(missing_fi!())?;
                    Some(els)
                }
                FI => None,
                _ => unreachable!(),
            };

            return Ok(builder::IfFragments {
                conditionals,
                else_branch: els,
            });
        }
    }

    /// Parses a single `for` command but does not parse any redirections that may follow.
    ///
    /// Since `for` is a compound command (and can have redirections applied to it) this
    /// method returns the relevant parts of the `for` command, without constructing an
    /// AST node, it so that the caller can do so with redirections.
    pub fn for_command(
        &mut self,
    ) -> ParseResult<builder::ForFragments<B::Command, B::Word>, B::Error> {
        let start_pos = self.iter.pos();
        self.reserved_word(&[FOR])
            .map_err(|_| self.make_unexpected_err())?;

        self.skip_whitespace();

        match self.iter.peek() {
            Some(&Name(_)) | Some(&Literal(_)) => {}
            _ => return Err(self.make_unexpected_err()),
        }

        let var_pos = self.iter.pos();
        let var = match self.iter.next() {
            Some(Name(v)) => v,
            Some(Literal(s)) => return Err(ParseError::new(BadIdent(s, var_pos))),
            _ => unreachable!(),
        };

        let var_comment = self.newline();
        let post_var_comments = self.linebreak();

        // A for command can take one of several different shapes (in pseudo regex syntax):
        // `for name [\n*] [in [word*]] [;\n* | \n+] do_group`
        // Below we'll disambiguate what situation we have as we move along.
        let (words, pre_body_comments) = if self.peek_reserved_word(&[IN]).is_some() {
            // Found `in` keyword, therefore we're looking at something like
            // `for name \n* in [words*] [;\n* | \n+] do_group`
            self.reserved_word(&[IN]).unwrap();

            let mut words = Vec::new();
            while let Some(w) = self.word()? {
                words.push(w);
            }

            let found_semi = eat_maybe!(self, {
                Semi => { true };
                _ => { false }
            });

            // We need either a newline or a ; to separate the words from the body
            // Thus if neither is found it is considered an error
            let words_comment = self.newline();
            if !found_semi && words_comment.is_none() {
                return Err(self.make_unexpected_err());
            }

            (
                Some((post_var_comments, words, words_comment)),
                self.linebreak(),
            )
        } else if Some(&Semi) == self.iter.peek() {
            // `for name \n*;\n* do_group`
            eat!(self, { Semi => {} });
            (None, self.linebreak())
        } else if self.peek_reserved_word(&[DO]).is_none() {
            // If we didn't find an `in` keyword, and we havent hit the body
            // (a `do` keyword), then we can reasonably say the script has
            // words without an `in` keyword.
            return Err(ParseError::new(IncompleteCmd(
                FOR,
                start_pos,
                IN,
                self.iter.pos(),
            )));
        } else {
            // `for name \n* do_group`
            (None, post_var_comments)
        };

        if self.peek_reserved_word(&[DO]).is_none() {
            return Err(ParseError::new(IncompleteCmd(
                FOR,
                start_pos,
                DO,
                self.iter.pos(),
            )));
        }

        let body = self.do_group()?;
        Ok(builder::ForFragments {
            var,
            var_comment,
            words,
            pre_body_comments,
            body,
        })
    }

    /// Parses a single `case` command but does not parse any redirections that may follow.
    ///
    /// Since `case` is a compound command (and can have redirections applied to it) this
    /// method returns the relevant parts of the `case` command, without constructing an
    /// AST node, it so that the caller can do so with redirections.
    pub fn case_command(
        &mut self,
    ) -> ParseResult<builder::CaseFragments<B::Command, B::Word>, B::Error> {
        let start_pos = self.iter.pos();

        macro_rules! missing_in {
            () => {
                |_| ParseError::new(IncompleteCmd(CASE, start_pos, IN, self.iter.pos()))
            };
        }

        macro_rules! missing_esac {
            () => {
                |_| ParseError::new(IncompleteCmd(CASE, start_pos, ESAC, self.iter.pos()))
            };
        }

        self.reserved_word(&[CASE])
            .map_err(|_| self.make_unexpected_err())?;

        let word = match self.word_preserve_trailing_whitespace_raw_with_delim(None, true)? {
            Some(w) => self.builder.word(w)?,
            None => return Err(self.make_unexpected_err()),
        };

        self.skip_whitespace();

        let post_word_comments = self.linebreak();
        self.reserved_word(&[IN]).map_err(missing_in!())?;
        let in_comment = self.newline();

        let mut pre_esac_comments = None;
        let mut arms = Vec::new();
        loop {
            let pre_pattern_comments = self.linebreak();
            if self.peek_reserved_word(&[ESAC]).is_some() {
                // Make sure we don't lose the captured comments if there are no body
                debug_assert_eq!(pre_esac_comments, None);
                pre_esac_comments = Some(pre_pattern_comments);
                break;
            }

            if let Some(&ParenOpen) = self.iter.peek() {
                self.iter.next();
            }

            // Make sure we check for missing `esac` here, otherwise if we have EOF
            // trying to parse a word will result in an `UnexpectedEOF` error
            if self.iter.peek().is_none() {
                return Err(()).map_err(missing_esac!());
            }

            self.stash_quote_context(QuoteContextKind::Other("patterns".into()));
            let mut patterns = Vec::new();
            loop {
                // only see the outermost opening quote.
                self.may_open_quote(Some(1), false);
                match self.word()? {
                    Some(p) => patterns.push(p),
                    None => return Err(self.make_unexpected_err()),
                }
                self.may_close_quote(None, false);
                self.skip_whitespace();

                match self.iter.peek() {
                    Some(&Pipe) => {
                        self.iter.next();
                        continue;
                    }

                    Some(&ParenClose) if !patterns.is_empty() => {
                        self.iter.next();
                        break;
                    }

                    // Make sure we check for missing `esac` here, otherwise if we have EOF
                    // trying to parse a word will result in an `UnexpectedEOF` error
                    None => return Err(()).map_err(missing_esac!()),
                    _ => return Err(self.make_unexpected_err()),
                }
            }
            self.may_close_quote(None, false);
            self.pop_quote_context();

            let pattern_comment = self.newline();
            let body = self.command_group_internal(CommandGroupDelimiters {
                reserved_words: &[ESAC],
                reserved_tokens: &[],
                exact_tokens: &[DSemi],
            })?;

            let (no_more_arms, arm_comment) = if Some(&DSemi) == self.iter.peek() {
                self.iter.next();
                (false, self.newline())
            } else {
                (true, None)
            };

            arms.push(builder::CaseArm {
                patterns: builder::CasePatternFragments {
                    pre_pattern_comments,
                    pattern_alternatives: patterns,
                    pattern_comment,
                },
                body,
                arm_comment,
            });

            if no_more_arms {
                break;
            }
        }

        let remaining_comments = self.linebreak();
        let pre_esac_comments = match pre_esac_comments {
            Some(mut comments) => {
                comments.extend(remaining_comments);
                comments
            }
            None => remaining_comments,
        };

        self.reserved_word(&[ESAC]).map_err(missing_esac!())?;

        Ok(builder::CaseFragments {
            word,
            post_word_comments,
            in_comment,
            arms,
            post_arms_comments: pre_esac_comments,
        })
    }

    /// Parses a m4 macro call if present. If no macro call is present,
    /// nothing is consumed from the token stream.
    pub fn maybe_macro_call(&mut self) -> Option<String> {
        if self.should_skip_macro_evaluation() {
            return None;
        }
        let names = m4_macro::MACROS
            .keys()
            .map(|s| s.as_str())
            .collect::<Vec<&str>>();
        let (quote_open, _) = self.get_quotes();
        if let Some(name) = self.peek_reserved_word_with_prefix(&names, Some(&quote_open)) {
            if m4_macro::get_macro(name).unwrap().1.num_args_required > 0 {
                // maybe the macro name is confusing (e.g. define).
                // so we additionally check if '(' follows.
                let mut m = self.iter.multipeek();
                m.peek_next(); // skip macro name
                if m.peek_next() == Some(&ParenOpen) {
                    Some(name.to_string())
                } else {
                    None
                }
            } else {
                Some(name.to_string())
            }
        } else if self.detect_user_macro {
            // check if user-defined macro call
            let found_quote_open = self.iter.peek() == Some(&quote_open);
            let mut peeked = self.iter.multipeek();
            if found_quote_open {
                peeked.peek_next();
            }
            if let Some(Name(name)) = peeked.peek_next() {
                let name = name.to_string();
                // @kui8shi
                // We ignore user-defined macro names with lower-case characters to
                // distinct the macro calls and shell command/function calls without any arguments.
                // It is just a heuristic.
                // They are indistinguishable especially in the case of no arguments.
                let is_macro_name = name.chars().next().is_some_and(|first| {
                    (first.is_ascii() && first.is_alphabetic() && first.is_uppercase())
                        || first == '_'
                }) && name.chars().skip(1).all(|c| {
                    (c.is_ascii() && c.is_alphabetic() && c.is_uppercase())
                        || c.is_numeric()
                        || c == '_'
                });
                if is_macro_name {
                    match peeked.peek_next() {
                        Some(&ParenOpen) | Some(&Newline) | None => Some(name),
                        _ => None,
                    }
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            None
        }
    }

    /// Crafts a macro call
    pub fn macro_call(&mut self, name_candidates: &[&str]) -> ParseResult<B::M4Macro, B::Error> {
        use m4_macro::{M4Argument, M4Type};
        let name = self
            .reserved_word(name_candidates)
            .map_err(|()| self.make_unexpected_err())?;
        let (name, macro_entry, original_name) = match m4_macro::get_macro(name) {
            Some((k, v, o)) => (k.as_ref(), Some(v), o),
            None => (name, None, None),
        };
        let mut effects = macro_entry
            .and_then(|sig| (!sig.has_no_exports()).then_some(sig))
            .map(|sig| sig.clone().into());
        if Some(&ParenOpen) == self.iter.peek() {
            eat!(self, { ParenOpen => {} });
        } else {
            return Ok(self.builder.macro_call(
                name.to_string(),
                Vec::new(),
                effects,
                original_name.map(|s| s.to_string()),
            )?);
        }
        self.stash_quote_context(QuoteContextKind::Macro(name.into()));
        let (_, quote_close) = self.get_quotes();
        let peeked_args = self.peek_macro_args()?;
        let num_args = peeked_args.len();
        let args = if let Some(signature) = macro_entry {
            let arg_types = &signature.arg_types;
            let _ret_type = signature.ret_type;
            let repeat = signature.repeat;
            let mut parsed_args = Vec::new();
            let mut idx_arg_type = 0;
            for (i, peeked_arg) in peeked_args.into_iter().enumerate() {
                self.linebreak();
                self.may_open_quote(None, false);
                let found_macro = self
                    .maybe_macro_call()
                    .and_then(|name| m4_macro::get_macro(&name).map(|(_, v, _)| v.clone()))
                    .is_some_and(|sig| {
                        sig.ret_type
                            .is_some_and(|ref t| t == &arg_types[idx_arg_type])
                    });
                let arg_type = if found_macro {
                    match &arg_types[idx_arg_type] {
                        &M4Type::Cmds => &M4Type::Cmds,
                        _ => &M4Type::Word,
                    }
                } else {
                    if arg_types.len() <= idx_arg_type {
                        dbg!(&name, self.iter.pos(), &self.quote_stack);
                        panic!();
                    }
                    &arg_types[idx_arg_type]
                };
                if matches!(&arg_type, M4Type::Cmds) {
                    // To consume commands empty but with whitespaces/newlines.
                    // `Parser::command_group` fails when parsing empty commands.
                    self.linebreak();
                }
                let arg = match arg_type {
                    M4Type::Lit | M4Type::AMCond => {
                        self.skip_macro_arg()?;
                        M4Argument::Literal(peeked_arg.clone())
                    }
                    M4Type::VarName(itself, f) => {
                        let arg = peeked_arg.clone();
                        if let Some(attr) = itself {
                            effects.get_or_insert_default().add_shell_var(&arg, attr);
                        }
                        if let Some(f) = f {
                            for (export_type, val) in f(&arg) {
                                effects
                                    .get_or_insert_default()
                                    .add_side_effect(&export_type, &val);
                            }
                        }
                        self.skip_macro_arg()?;
                        M4Argument::Literal(arg)
                    }
                    M4Type::Type(f) => {
                        let arg = peeked_arg.clone();
                        if let Some(f) = f {
                            for (export_type, val) in f(&arg) {
                                effects
                                    .get_or_insert_default()
                                    .add_side_effect(&export_type, &val);
                            }
                        }
                        self.skip_macro_arg()?;
                        M4Argument::Literal(arg)
                    }
                    // FIXME: Macro body should be parsed as the same type as its return type.
                    M4Type::Prog => {
                        self.skip_macro_arg()?;
                        M4Argument::Program(peeked_arg.clone())
                    }
                    M4Type::CPP => {
                        if let Some(word) = self.word_preserve_trailing_whitespace_raw_with_delim(
                            Some(&[Comma, ParenClose]),
                            false,
                        )? {
                            M4Argument::Word(self.builder.word(word)?)
                        } else {
                            self.skip_macro_arg()?;
                            M4Argument::Literal(peeked_arg.clone())
                        }
                    }
                    M4Type::Word => {
                        self.skip_whitespace();
                        if self.in_quote() {
                            let words = self.word_interpolated_raw(
                                Some(&[quote_close.clone()]),
                                self.iter.pos(),
                            )?;
                            M4Argument::Word(
                                self.builder
                                    .word(Concat(words.into_iter().map(Simple).collect()))?,
                            )
                        } else if let Some(&Comma | &ParenClose) = self.iter.peek() {
                            // we do not found any effective words.
                            // empty arguments like '[]' were already skipped by `Parser::linebreak()`.
                            M4Argument::Literal("".into())
                        } else if let Some(word) = self
                            .word_preserve_trailing_whitespace_raw_with_delim(
                                Some(&[Comma]),
                                false,
                            )?
                        {
                            M4Argument::Word(self.builder.word(word)?)
                        } else {
                            self.skip_macro_arg()?;
                            M4Argument::Literal(peeked_arg.clone())
                        }
                    }
                    M4Type::Library(f) => {
                        if self.in_quote() {
                            let words = self.word_interpolated_raw(
                                Some(&[quote_close.clone()]),
                                self.iter.pos(),
                            )?;
                            M4Argument::Word(
                                self.builder
                                    .word(Concat(words.into_iter().map(Simple).collect()))?,
                            )
                        } else if let Some(word) = self
                            .word_preserve_trailing_whitespace_raw_with_delim(
                                Some(&[Comma]),
                                false,
                            )?
                        {
                            // TODO: How can we track side effects like defining `${var}_suffix`?
                            M4Argument::Word(self.builder.word(word)?)
                        } else {
                            let arg = peeked_arg.clone();
                            if let Some(f) = f {
                                for (export_type, val) in f(&arg) {
                                    effects
                                        .get_or_insert_default()
                                        .add_side_effect(&export_type, &val);
                                }
                            }
                            M4Argument::Literal(arg)
                        }
                    }
                    M4Type::Path(f) | M4Type::Symbol(f) => {
                        if let Some(word) = self.word_preserve_trailing_whitespace_raw_with_delim(
                            Some(&[Comma]),
                            false,
                        )? {
                            M4Argument::Word(self.builder.word(word)?)
                        } else {
                            let arg = peeked_arg.clone();
                            if let Some(f) = f {
                                for (export_type, val) in f(&arg) {
                                    effects
                                        .get_or_insert_default()
                                        .add_side_effect(&export_type, &val);
                                }
                            }
                            M4Argument::Literal(arg)
                        }
                    }
                    M4Type::Arr(delim) => {
                        let end = if i < num_args - 1 {
                            // array should end with comma
                            &Comma
                        } else {
                            // array should end with ')'
                            &ParenClose
                        };
                        self.macro_arg_array(delim, end, &mut effects, None)?
                    }
                    M4Type::Symbols(delim, f)
                    | M4Type::Paths(delim, f)
                    | M4Type::Types(delim, f) => {
                        let end = if i < num_args - 1 {
                            // array should end with comma
                            &Comma
                        } else {
                            // array should end with ')'
                            &ParenClose
                        };
                        self.macro_arg_array(delim, end, &mut effects, *f)?
                    }
                    M4Type::Cmds => {
                        self.may_close_quote(None, false);
                        if let Some(&Comma | &ParenClose) = self.iter.peek() {
                            // we do not found any effective commands.
                            // empty arguments like '[]' were already skipped by `Parser::linebreak()`.
                            M4Argument::Commands(Vec::new())
                        } else {
                            let delims = if i < num_args - 1 {
                                // argument should end with comma
                                &[Comma, quote_close.clone()]
                            } else {
                                // argument should end with ')'
                                &[ParenClose, quote_close.clone()]
                            };
                            let cmds = self
                                .command_group(CommandGroupDelimiters {
                                    exact_tokens: delims,
                                    ..Default::default()
                                })?
                                .commands;
                            M4Argument::Commands(cmds)
                        }
                    }
                    M4Type::Body => {
                        self.skip_macro_arg()?;
                        // we really wanted to analyze the type of macro body
                        // but it needs a lot of analysis on the entire script
                        // such work should be delegated to later process. so leave unknown.
                        M4Argument::Unknown(peeked_arg.clone())
                    }
                    unknown => {
                        eprintln!("Unexpected M4Type for arguments: {:?}", unknown);
                        return Err(self.make_unexpected_err());
                    }
                };
                self.linebreak();
                self.may_close_quote(None, false);
                self.linebreak();
                eat!(self, { Comma => {}, ParenClose => {} });
                idx_arg_type += 1;
                if let Some((start, end)) = repeat {
                    // repeat in [start..=end]
                    let remain_size = num_args - i - 1;
                    let repeat_size = end - start + 1;
                    if (idx_arg_type > end) && remain_size >= repeat_size {
                        idx_arg_type = start
                    }
                }
                parsed_args.push(arg);
            }
            parsed_args
        } else {
            // user-defined macro arguments' types are unknown
            while let Comma = self.skip_macro_arg()? {
                eat!(self, {Comma => {}});
            }
            self.linebreak();
            eat!(self, {ParenClose => {}});
            peeked_args.into_iter().map(M4Argument::Unknown).collect()
        };
        assert!(!self.in_quote());
        self.pop_quote_context();
        // A whitespace is not allowed between m4 macro name and the opening parenthesis
        // eat!(self, { ParenOpen => {} });
        Ok(self.builder.macro_call(
            name.to_string(),
            args,
            effects,
            original_name.map(|s| s.to_string()),
        )?)
    }

    // multipeek does not have pos() trait
    fn skip_macro_arg(&mut self) -> ParseResult<Token, B::Error> {
        let mut unquoted_paren_depth = 0;
        loop {
            self.may_open_quote(None, false);
            self.may_close_quote(None, false);
            let in_quote = self.in_quote();
            match self.iter.peek() {
                Some(&Comma) if !in_quote && unquoted_paren_depth == 0 => return Ok(Comma),
                Some(&ParenOpen) if !in_quote => {
                    unquoted_paren_depth += 1;
                }
                Some(&ParenClose) if !in_quote => {
                    if unquoted_paren_depth < 1 {
                        return Ok(ParenClose);
                    }
                    unquoted_paren_depth -= 1;
                }
                Some(_) => (),
                None => return Err(ParseError::new(UnexpectedEOF)),
            }
            // consume token
            self.iter.next();
        }
    }

    fn macro_arg_array(
        &mut self,
        delim: &ArrayDelim,
        end: &Token,
        effects: &mut Option<SideEffect>,
        func: Option<M4ExportFunc>,
    ) -> ParseResult<M4Argument<B::Command, B::Word>, B::Error> {
        let (_, quote_close) = self.get_quotes();
        let mut arr = Vec::new();
        // Parse array argument
        loop {
            self.skip_whitespace();
            // @kui8shi
            // FIXME: For brevity of the implementation, we include any tokens
            // that is taken to be an end of shell word (e.g. parenthesis)
            // to the delimiters of the array.
            let word = match delim {
                ArrayDelim::Blank => {
                    let delims = &[Whitespace(" ".into()), Newline, end.clone()];
                    eat_maybe!(self, { Newline => { self.iter.next(); } });
                    self.word_preserve_trailing_whitespace_raw_with_delim(Some(delims), false)?
                }
                ArrayDelim::Comma => {
                    eat_maybe!(self, { Newline => { self.iter.next(); } });
                    let mut words =
                        self.word_interpolated_raw(Some(&[Comma, end.clone()]), self.iter.pos())?;
                    if words.len() == 0 {
                        None
                    } else if words.len() == 1 {
                        Some(Single(Simple(words.pop().unwrap())))
                    } else {
                        Some(Concat(words.into_iter().map(Simple).collect()))
                    }
                }
            };
            if let Some(word) = word {
                if let Some(f) = func {
                    if let Single(Simple(ref frag)) = word {
                        if let Some(elm) = frag.clone().into() {
                            for (export_type, val) in f(&elm) {
                                effects
                                    .get_or_insert_default()
                                    .add_side_effect(&export_type, &val);
                            }
                        }
                    }
                }
                // empty words are ignored (the case of None).
                arr.push(self.builder.word(word)?);
            }
            let in_quote = self.in_quote();
            if let Some(tok) = self.iter.peek() {
                if tok == &quote_close {
                    self.iter.next();
                    *self.quote_level() -= 1;
                    self.skip_whitespace();
                    if self.iter.peek() == Some(end) {
                        break;
                    } else {
                        return Err(self.make_unexpected_err());
                    }
                } else if tok == end && !in_quote {
                    break;
                } else if matches!(tok, Comma | Newline) && in_quote {
                    self.iter.next();
                }
            }
        }
        self.may_close_quote(None, false);
        Ok(M4Argument::Array(arr))
    }

    /// Parse arbitrary arguments of m4 macro call as raw literals,
    /// The literal contains all chracters except leading/trailing whitespaces.
    fn peek_macro_args(&mut self) -> ParseResult<Vec<String>, B::Error> {
        let (quote_open, quote_close) = self.get_quotes();
        let mut peeked = self.iter.multipeek();
        let mut quote_level = 0;
        let mut unquoted_paren_level = 0;
        let mut literals = Vec::new();
        let mut buf = String::new();
        loop {
            match peeked.peek_next() {
                Some(&Comma) if quote_level == 0 && unquoted_paren_level == 0 => {
                    literals.push(buf.trim().to_string());
                    buf = String::new();
                }
                Some(&ParenOpen) if quote_level == 0 => {
                    unquoted_paren_level += 1;
                    if unquoted_paren_level >= 1 {
                        buf.push_str(ParenOpen.as_str());
                    }
                }
                Some(&ParenClose) if quote_level == 0 => {
                    if unquoted_paren_level < 1 {
                        literals.push(buf.trim().to_string());
                        return Ok(literals);
                    } else {
                        buf.push_str(ParenClose.as_str());
                    }
                    unquoted_paren_level -= 1;
                }
                Some(Newline | Whitespace(_)) if buf.is_empty() => continue,
                Some(EmptyQuotes) if quote_level == 0 => continue,
                Some(tok) => {
                    if tok == &quote_open {
                        quote_level += 1;
                        if quote_level == 1 {
                            // The quote is outermost
                            continue;
                        }
                    } else if tok == &quote_close {
                        quote_level -= 1;
                        if quote_level == 0 {
                            // The quote is outermost
                            continue;
                        }
                    }
                    buf.push_str(tok.as_str());
                }
                None => return Err(ParseError::new(UnexpectedEOF)),
            }
        }
    }

    /// Parses a single function declaration if present. If no function is present,
    /// nothing is consumed from the token stream.
    pub fn maybe_function_declaration(
        &mut self,
    ) -> ParseResult<Option<B::PipeableCommand>, B::Error> {
        if self.peek_reserved_word(&[FUNCTION]).is_some() {
            return self.function_declaration().map(Some);
        }

        let is_fn = {
            let mut peeked = self.iter.multipeek();
            if let Some(&Name(_)) = peeked.peek_next() {
                match peeked.peek_next() {
                    Some(&Whitespace(_)) => Some(&ParenOpen) == peeked.peek_next(),
                    // @kui8shi
                    // We expect whitespaces between the function name and the parentheses to distinct the function declaration from m4 macro calls.
                    // It does not adhere to the standard but I think this simple distinction works in most cases.
                    _ => false,
                }
            } else {
                false
            }
        };

        if is_fn {
            self.function_declaration().map(Some)
        } else {
            Ok(None)
        }
    }

    /// Parses a single function declaration.
    ///
    /// A function declaration must either begin with the `function` reserved word, or
    /// the name of the function must be followed by `()`. Whitespace is allowed between
    /// the name and `(`, and whitespace is allowed between `()`.
    /// @kui8shi
    /// We disallowed whitespace between '()' for the distinction from m4 macro calls.
    pub fn function_declaration(&mut self) -> ParseResult<B::PipeableCommand, B::Error> {
        let (name, post_name_comments, body) = self.function_declaration_internal()?;
        Ok(self
            .builder
            .function_declaration(name, post_name_comments, body)?)
    }

    /// Like `Parser::function_declaration`, but does not pass the result to the builder
    fn function_declaration_internal(
        &mut self,
    ) -> ParseResult<(String, Vec<builder::Newline>, B::CompoundCommand), B::Error> {
        let found_fn = match self.peek_reserved_word(&[FUNCTION]) {
            Some(_) => {
                self.iter.next();
                true
            }
            None => false,
        };

        self.skip_whitespace();

        match self.iter.peek() {
            Some(&Name(_)) | Some(&Literal(_)) => {}
            _ => return Err(self.make_unexpected_err()),
        }

        let ident_pos = self.iter.pos();
        let name = match self.iter.next() {
            Some(Name(n)) => n,
            Some(Literal(s)) => return Err(ParseError::new(BadIdent(s, ident_pos))),
            _ => unreachable!(),
        };

        // If there is no whitespace after the function name, the only valid
        // possibility is for `()` to appear.
        let body = if Some(&ParenOpen) == self.iter.peek() {
            eat!(self, { ParenOpen => {} });
            self.skip_whitespace();
            eat!(self, { ParenClose => {} });
            None
        } else if found_fn && Some(&Newline) == self.iter.peek() {
            // Do nothing, function declaration satisfied
            None
        } else {
            // Enforce at least one whitespace between function declaration and body
            eat!(self, { Whitespace(_) => {} });
            self.skip_whitespace();

            // If we didn't find the function keyword, we MUST find `()` at this time
            if !found_fn {
                eat!(self, { ParenOpen => {} });
                self.skip_whitespace();
                eat!(self, { ParenClose => {} });
                None
            } else if Some(&ParenOpen) == self.iter.peek() {
                // Otherwise it is possible for there to be a subshell as the body
                let subshell = self.subshell_internal(true)?;
                if subshell.commands.is_empty() && subshell.trailing_comments.is_empty() {
                    // Case like `function foo () ...`
                    None
                } else {
                    // Case like `function foo (subshell)`
                    Some(self.builder.subshell(subshell, Vec::new())?)
                }
            } else {
                None
            }
        };

        let (post_name_comments, body) = match body {
            Some(subshell) => (Vec::new(), subshell),
            None => (self.linebreak(), self.compound_command()?),
        };

        Ok((name, post_name_comments, body))
    }

    /// Skips over any encountered whitespace but preserves newlines.
    #[inline]
    pub fn skip_whitespace(&mut self) {
        loop {
            while matches!(self.iter.peek(), Some(&Whitespace(_)) | Some(&EmptyQuotes)) {
                self.iter.next();
            }

            let found_backslash_newline = {
                let mut peeked = self.iter.multipeek();
                Some(&Backslash) == peeked.peek_next() && Some(&Newline) == peeked.peek_next()
            };

            if found_backslash_newline {
                self.iter.next();
                self.iter.next();
            } else {
                break;
            }
        }
    }

    /// Parses zero or more `Token::Newline`s, skipping whitespace but capturing comments.
    #[inline]
    pub fn linebreak(&mut self) -> Vec<builder::Newline> {
        let mut lines = Vec::new();
        while let Some(n) = self.newline() {
            lines.push(n);
        }
        lines
    }

    /// Tries to parse a `Token::Newline` (or a comment) after skipping whitespace.
    pub fn newline(&mut self) -> Option<builder::Newline> {
        self.skip_whitespace();

        match self.iter.peek() {
            Some(&Pound) => {
                let comment = self
                    .iter
                    .by_ref()
                    .take_while(|t| t != &Newline)
                    .collect::<Vec<_>>();
                Some(builder::Newline(Some(concat_tokens(&comment))))
            }

            // @kui8shi
            // Treats any line which starts with the word "dnl" as a comment.
            Some(Name(s)) if s == "dnl" => {
                let comment = self
                    .iter
                    .by_ref()
                    .take_while(|t| t != &Newline)
                    .collect::<Vec<_>>();
                Some(builder::Newline(Some(concat_tokens(&comment))))
            }

            Some(&Newline) => {
                self.iter.next();
                Some(builder::Newline(None))
            }

            _ => None,
        }
    }

    /// Checks that one of the specified tokens appears as a reserved word.
    ///
    /// The token must be followed by a token which delimits a word when it is
    /// unquoted/unescaped.
    ///
    /// If a reserved word is found, the token which it matches will be
    /// returned in case the caller cares which specific reserved word was found.
    pub fn peek_reserved_token<'t>(&mut self, tokens: &'t [Token]) -> Option<&'t Token> {
        if tokens.is_empty() {
            return None;
        }

        let care_about_whitespace = tokens.iter().any(|tok| {
            if let Whitespace(_) = *tok {
                true
            } else {
                false
            }
        });

        // If the caller cares about whitespace as a reserved word we should
        // do a reserved word check without skipping any leading whitespace.
        // If we don't find anything the first time (or if the caller does
        // not care about whitespace tokens) we will skip the whitespace
        // and try again.
        let num_tries = if care_about_whitespace {
            2
        } else {
            self.skip_whitespace();
            1
        };

        for _ in 0..num_tries {
            {
                let mut peeked = self.iter.multipeek();

                let found_tok = match peeked.peek_next() {
                    Some(tok) => tokens.iter().find(|&t| t == tok),
                    None => return None,
                };

                if let ret @ Some(_) = found_tok {
                    let found_delim = match peeked.peek_next() {
                        Some(delim) => delim.is_word_delimiter(),
                        None => true, // EOF is also a valid delimeter
                    };

                    if found_delim {
                        return ret;
                    }
                }
            }

            self.skip_whitespace();
        }

        None
    }

    /// Checks that one of the specified strings appears as a reserved word.
    ///
    /// The word must appear as a single token, unquoted and unescaped, and
    /// must be followed by a token which delimits a word when it is
    /// unquoted/unescaped. The reserved word may appear as a `Token::Name`
    /// or a `Token::Literal`.
    ///
    /// If a reserved word is found, the string which it matches will be
    /// returned in case the caller cares which specific reserved word was found.
    pub fn peek_reserved_word<'t>(&mut self, words: &'t [&str]) -> Option<&'t str> {
        self.peek_reserved_word_with_prefix(words, None)
    }

    /// It is identical to peek_reserved_word.
    /// But allow a prefix token before the reserved word appears.
    pub fn peek_reserved_word_with_prefix<'t>(
        &mut self,
        words: &'t [&str],
        prefix: Option<&Token>,
    ) -> Option<&'t str> {
        if words.is_empty() {
            return None;
        }

        self.skip_whitespace();
        let found_prefix = self.iter.peek() == prefix;
        let mut peeked = self.iter.multipeek();
        if found_prefix {
            peeked.peek_next();
        }
        let found_tok = match peeked.peek_next() {
            Some(&Name(ref kw)) | Some(&Literal(ref kw)) => {
                words.iter().find(|&w| w == kw).copied()
            }
            _ => None,
        };

        match peeked.peek_next() {
            Some(delim) if delim.is_word_delimiter() => found_tok,
            None => found_tok, // EOF is a valid delimeter
            _ => None,
        }
    }

    /// Checks that one of the specified tokens appears as a reserved word
    /// and consumes it, returning the token it matched in case the caller
    /// cares which specific reserved word was found.
    pub fn reserved_token(&mut self, tokens: &[Token]) -> ParseResult<Token, B::Error> {
        match self.peek_reserved_token(tokens) {
            Some(_) => Ok(self.iter.next().unwrap()),
            None => {
                // If the desired token is next, but we failed to find a reserved
                // token (because the token after it isn't a valid delimeter)
                // then the following token is the unexpected culprit, so we should
                // skip the upcoming one before forming the error.
                let skip_one = {
                    let peeked = self.iter.peek();
                    tokens.iter().any(|t| Some(t) == peeked)
                };

                if skip_one {
                    self.iter.next();
                }
                Err(self.make_unexpected_err())
            }
        }
    }

    /// Checks that one of the specified strings appears as a reserved word
    /// and consumes it, returning the string it matched in case the caller
    /// cares which specific reserved word was found.
    pub fn reserved_word<'t>(&mut self, words: &'t [&str]) -> Result<&'t str, ()> {
        match self.peek_reserved_word(words) {
            Some(s) => {
                self.iter.next();
                Ok(s)
            }
            None => Err(()),
        }
    }

    /// Parses commands until a configured delimeter (or EOF)
    /// is reached, without consuming the token or reserved word.
    ///
    /// Any reserved word/token **must** appear after a complete command
    /// separator (e.g. `;`, `&`, or a newline), otherwise it will be
    /// parsed as part of the command.
    ///
    /// It is considered an error if no commands are present.
    pub fn command_group(
        &mut self,
        cfg: CommandGroupDelimiters<'_, '_, '_>,
    ) -> ParseResult<builder::CommandGroup<B::Command>, B::Error> {
        self.may_open_quote(None, false);
        let group = self.command_group_internal(cfg)?;
        self.may_close_quote(None, false);
        if group.commands.is_empty() {
            Err(self.make_unexpected_err())
        } else {
            Ok(group)
        }
    }

    /// Like `compound_list`, but allows for the list of commands to be empty.
    fn command_group_internal(
        &mut self,
        cfg: CommandGroupDelimiters<'_, '_, '_>,
    ) -> ParseResult<builder::CommandGroup<B::Command>, B::Error> {
        let found_delim = |slf: &mut AutoconfParser<_, _>| {
            let found_exact = !cfg.exact_tokens.is_empty()
                && slf
                    .iter
                    .peek()
                    .map(|peeked| cfg.exact_tokens.iter().any(|tok| tok == peeked))
                    .unwrap_or(false);

            found_exact
                || slf.peek_reserved_word(cfg.reserved_words).is_some()
                || slf.peek_reserved_token(cfg.reserved_tokens).is_some()
        };

        let mut cmds = Vec::new();
        let mut trailing_comments = Vec::new();

        loop {
            if self.is_closing_quote() && !self.may_close_quote(None, false) {
                // in most cases, quotes encountering here is strange ones.
                // we alywas unwrap the quotes around the command independent of quote context
                self.iter.next();
            }

            let start_pos = self.iter.pos();

            let leading_comments = self.linebreak();

            if found_delim(self) || self.iter.peek().is_none() {
                debug_assert!(trailing_comments.is_empty());
                trailing_comments = leading_comments;
                break;
            }

            if self.is_opening_quote() && !self.may_open_quote(None, false) {
                // in most cases, quotes encountering here is strange ones.
                // we always unwrap the quotes around the command independent of quote context
                self.iter.next();
            }

            cmds.push(self.complete_command_with_leading_comments(leading_comments, start_pos)?);
        }

        Ok(builder::CommandGroup {
            commands: cmds,
            trailing_comments,
        })
    }

    /// Parses the body of any arbitrary arithmetic expression, e.g. `x + $y << 5`.
    /// The caller is responsible for parsing the external `$(( ))` tokens.
    pub fn arithmetic_substitution(&mut self) -> ParseResult<DefaultArithmetic, B::Error> {
        let mut exprs = Vec::new();
        loop {
            self.skip_whitespace();
            exprs.push(self.arith_assig()?);

            eat_maybe!(self, {
                Comma => {};
                _ => { break },
            });
        }

        if exprs.len() == 1 {
            Ok(exprs.pop().unwrap())
        } else {
            Ok(ast::Arithmetic::Sequence(exprs))
        }
    }

    /// Parses expressions such as `var = expr` or `var op= expr`, where `op` is
    /// any of the following operators: *, /, %, +, -, <<, >>, &, |, ^.
    fn arith_assig(&mut self) -> ParseResult<DefaultArithmetic, B::Error> {
        use crate::ast::Arithmetic::*;

        self.skip_whitespace();

        let assig = {
            let mut assig = false;
            let mut peeked = self.iter.multipeek();

            'assig_check: loop {
                match peeked.peek_next() {
                    Some(&Dollar) => continue, // Skip Dollar and peek next
                    Some(&Name(_)) => loop {
                        match peeked.peek_next() {
                            Some(&Whitespace(_)) => continue, // Skip whitespace and peek next
                            Some(&Star) | Some(&Slash) | Some(&Percent) | Some(&Plus)
                            | Some(&Dash) | Some(&DLess) | Some(&DGreat) | Some(&Amp)
                            | Some(&Pipe) | Some(&Caret) => {
                                assig = Some(&Equals) == peeked.peek_next()
                            }

                            // Make sure we only recognize $(( x = ...)) but NOT $(( x == ...))
                            Some(&Equals) => assig = Some(&Equals) != peeked.peek_next(),
                            _ => {}
                        }

                        break 'assig_check;
                    },
                    _ => break 'assig_check,
                }
            }

            assig
        };

        if !assig {
            return self.arith_ternary();
        }

        let var = self.arith_var()?;
        self.skip_whitespace();
        let op = match self.iter.next() {
            Some(op @ Star) | Some(op @ Slash) | Some(op @ Percent) | Some(op @ Plus)
            | Some(op @ Dash) | Some(op @ DLess) | Some(op @ DGreat) | Some(op @ Amp)
            | Some(op @ Pipe) | Some(op @ Caret) => {
                eat!(self, { Equals => {} });
                op
            }
            Some(op @ Equals) => op,
            _ => unreachable!(),
        };

        let value = Box::new(self.arith_assig()?);
        let expr = match op {
            Star => Box::new(Mult(Box::new(Var(var.clone())), value)),
            Slash => Box::new(Div(Box::new(Var(var.clone())), value)),
            Percent => Box::new(Modulo(Box::new(Var(var.clone())), value)),
            Plus => Box::new(Add(Box::new(Var(var.clone())), value)),
            Dash => Box::new(Sub(Box::new(Var(var.clone())), value)),
            DLess => Box::new(ShiftLeft(Box::new(Var(var.clone())), value)),
            DGreat => Box::new(ShiftRight(Box::new(Var(var.clone())), value)),
            Amp => Box::new(BitwiseAnd(Box::new(Var(var.clone())), value)),
            Pipe => Box::new(BitwiseOr(Box::new(Var(var.clone())), value)),
            Caret => Box::new(BitwiseXor(Box::new(Var(var.clone())), value)),
            Equals => value,
            _ => unreachable!(),
        };
        Ok(Assign(var, expr))
    }

    /// Parses expressions such as `expr ? expr : expr`.
    fn arith_ternary(&mut self) -> ParseResult<DefaultArithmetic, B::Error> {
        let guard = self.arith_logical_or()?;
        self.skip_whitespace();
        eat_maybe!(self, {
            Question => {
                let body = self.arith_ternary()?;
                self.skip_whitespace();
                eat!(self, { Colon => {} });
                let els = self.arith_ternary()?;
                Ok(ast::Arithmetic::Ternary(Box::new(guard), Box::new(body), Box::new(els)))
            };
            _ => { Ok(guard) },
        })
    }

    arith_parse!(
        /// Parses expressions such as `expr || expr`.
        fn arith_logical_or,
        arith_logical_and,
        OrIf  => ast::Arithmetic::LogicalOr
    );

    arith_parse!(
        /// Parses expressions such as `expr && expr`.
        fn arith_logical_and,
        arith_bitwise_or,
        AndIf => ast::Arithmetic::LogicalAnd
    );

    arith_parse!(
        /// Parses expressions such as `expr | expr`.
        fn arith_bitwise_or,
        arith_bitwise_xor,
        Pipe  => ast::Arithmetic::BitwiseOr
    );

    arith_parse!(
        /// Parses expressions such as `expr ^ expr`.
        fn arith_bitwise_xor,
        arith_bitwise_and,
        Caret => ast::Arithmetic::BitwiseXor
    );

    arith_parse!(
        /// Parses expressions such as `expr & expr`.
        fn arith_bitwise_and,
        arith_eq,
        Amp   => ast::Arithmetic::BitwiseAnd
    );

    /// Parses expressions such as `expr == expr` or `expr != expr`.
    #[inline]
    fn arith_eq(&mut self) -> ParseResult<DefaultArithmetic, B::Error> {
        let mut expr = self.arith_ineq()?;
        loop {
            self.skip_whitespace();
            let eq_type = eat_maybe!(self, {
                Equals => { true },
                Bang => { false };
                _ => { break }
            });

            eat!(self, { Equals => {} });
            let next = self.arith_ineq()?;
            expr = if eq_type {
                ast::Arithmetic::Eq(Box::new(expr), Box::new(next))
            } else {
                ast::Arithmetic::NotEq(Box::new(expr), Box::new(next))
            };
        }
        Ok(expr)
    }

    /// Parses expressions such as `expr < expr`,`expr <= expr`,`expr > expr`,`expr >= expr`.
    #[inline]
    fn arith_ineq(&mut self) -> ParseResult<DefaultArithmetic, B::Error> {
        let mut expr = self.arith_shift()?;
        loop {
            self.skip_whitespace();
            eat_maybe!(self, {
                Less => {
                    let eq = eat_maybe!(self, { Equals => { true }; _ => { false } });
                    let next = self.arith_shift()?;

                    expr = if eq {
                        ast::Arithmetic::LessEq(Box::new(expr), Box::new(next))
                    } else {
                        ast::Arithmetic::Less(Box::new(expr), Box::new(next))
                    };
                },
                Great => {
                    let eq = eat_maybe!(self, { Equals => { true }; _ => { false } });
                    let next = self.arith_shift()?;

                    expr = if eq {
                        ast::Arithmetic::GreatEq(Box::new(expr), Box::new(next))
                    } else {
                        ast::Arithmetic::Great(Box::new(expr), Box::new(next))
                    };
                };
                _ => { break },
            });
        }
        Ok(expr)
    }

    arith_parse!(
        /// Parses expressions such as `expr << expr` or `expr >> expr`.
        fn arith_shift,
        arith_add,
        DLess  => ast::Arithmetic::ShiftLeft,
        DGreat => ast::Arithmetic::ShiftRight
    );

    arith_parse!(
        /// Parses expressions such as `expr + expr` or `expr - expr`.
        fn arith_add,
        arith_mult,
        Plus => ast::Arithmetic::Add,
        Dash => ast::Arithmetic::Sub
    );

    arith_parse!(
        /// Parses expressions such as `expr * expr`, `expr / expr`, or `expr % expr`.
        fn arith_mult,
        arith_pow,
        Star    => ast::Arithmetic::Mult,
        Slash   => ast::Arithmetic::Div,
        Percent => ast::Arithmetic::Modulo
    );

    /// Parses expressions such as `expr ** expr`.
    fn arith_pow(&mut self) -> ParseResult<DefaultArithmetic, B::Error> {
        let expr = self.arith_unary_misc()?;
        self.skip_whitespace();

        // We must be extra careful here because ** has a higher precedence
        // than *, meaning power operations will be parsed before multiplication.
        // Thus we should be absolutely certain we should parse a ** operator
        // and avoid confusing it with a multiplication operation that is yet
        // to be parsed.
        let double_star = {
            let mut peeked = self.iter.multipeek();
            peeked.peek_next() == Some(&Star) && peeked.peek_next() == Some(&Star)
        };

        if double_star {
            eat!(self, { Star => {} });
            eat!(self, { Star => {} });
            Ok(ast::Arithmetic::Pow(
                Box::new(expr),
                Box::new(self.arith_pow()?),
            ))
        } else {
            Ok(expr)
        }
    }

    /// Parses expressions such as `!expr`, `~expr`, `+expr`, `-expr`, `++var` and `--var`.
    fn arith_unary_misc(&mut self) -> ParseResult<DefaultArithmetic, B::Error> {
        self.skip_whitespace();
        let expr = eat_maybe!(self, {
            Bang  => { ast::Arithmetic::LogicalNot(Box::new(self.arith_unary_misc()?)) },
            Tilde => { ast::Arithmetic::BitwiseNot(Box::new(self.arith_unary_misc()?)) },
            Plus  => {
                eat_maybe!(self, {
                    // Although we can optimize this out, we'll let the AST builder handle
                    // optimizations, in case it is interested in such redundant situations.
                    Dash => {
                        let next = self.arith_unary_misc()?;
                        ast::Arithmetic::UnaryPlus(Box::new(ast::Arithmetic::UnaryMinus(Box::new(next))))
                    },
                    Plus => { ast::Arithmetic::PreIncr(self.arith_var()?) };
                    _ => { ast::Arithmetic::UnaryPlus(Box::new(self.arith_unary_misc()?)) }
                })
            },

            Dash  => {
                eat_maybe!(self, {
                    // Although we can optimize this out, we'll let the AST builder handle
                    // optimizations, in case it is interested in such redundant situations.
                    Plus => {
                        let next = self.arith_unary_misc()?;
                        ast::Arithmetic::UnaryMinus(Box::new(ast::Arithmetic::UnaryPlus(Box::new(next))))
                    },
                    Dash => { ast::Arithmetic::PreDecr(self.arith_var()?) };
                    _ => { ast::Arithmetic::UnaryMinus(Box::new(self.arith_unary_misc()?)) }
                })
            };

            _ => { self.arith_post_incr()? }
        });
        Ok(expr)
    }

    /// Parses expressions such as `(expr)`, numeric literals, `var`, `var++`, or `var--`.
    /// Numeric literals must appear as a single `Literal` token. `Name` tokens will be
    /// treated as variables.
    #[inline]
    fn arith_post_incr(&mut self) -> ParseResult<DefaultArithmetic, B::Error> {
        self.skip_whitespace();
        eat_maybe!(self, {
            ParenOpen => {
                let expr = self.arithmetic_substitution()?;
                self.skip_whitespace();
                eat!(self, { ParenClose => {} });
                return Ok(expr);
            }
        });

        let num = if let Some(Literal(s)) = self.iter.peek() {
            if s.starts_with("0x") || s.starts_with("0X") {
                // from_str_radix does not like it when 0x is present
                // in the string to parse, thus we should strip it off.
                // Also, if the string is empty from_str_radix will return
                // an error; shells like bash and zsh treat `0x` as `0x0`
                // so we will do the same.
                let num = &s[2..];
                if num.is_empty() {
                    Some(0)
                } else {
                    isize::from_str_radix(&s[2..], 16).ok()
                }
            } else if s.starts_with('0') {
                isize::from_str_radix(s, 8).ok()
            } else {
                isize::from_str_radix(s, 10).ok()
            }
        } else {
            None
        };

        let expr = match num {
            Some(num) => {
                // Make sure we consume the Token::Literal which holds the number
                self.iter.next();
                ast::Arithmetic::Literal(num)
            }
            None => {
                let var = self.arith_var()?;

                // We must be extra careful here because post-increment has a higher precedence
                // than addition/subtraction meaning post-increment operations will be parsed
                // before addition. Thus we should be absolutely certain we should parse a
                // post-increment operator and avoid confusing it with an addition operation
                // that is yet to be parsed.
                let post_incr = {
                    self.skip_whitespace();
                    let mut peeked = self.iter.multipeek();
                    match peeked.peek_next() {
                        Some(&Plus) => peeked.peek_next() == Some(&Plus),
                        Some(&Dash) => peeked.peek_next() == Some(&Dash),
                        _ => false,
                    }
                };

                if post_incr {
                    eat!(self, {
                        Plus => { eat!(self, { Plus => { ast::Arithmetic::PostIncr(var) } }) },
                        Dash => { eat!(self, { Dash => { ast::Arithmetic::PostDecr(var) } }) },
                    })
                } else {
                    ast::Arithmetic::Var(var)
                }
            }
        };
        Ok(expr)
    }

    /// Parses a variable name in the form `name` or `$name`.
    #[inline]
    fn arith_var(&mut self) -> ParseResult<String, B::Error> {
        self.skip_whitespace();
        eat_maybe!(self, { Dollar => {} });

        if let Some(&Name(_)) = self.iter.peek() {
            if let Some(Name(n)) = self.iter.next() {
                Ok(n)
            } else {
                unreachable!()
            }
        } else {
            Err(self.make_unexpected_err())
        }
    }
}

fn concat_tokens(tokens: &[Token]) -> String {
    let len = tokens.iter().fold(0, |len, t| len + t.len());
    let mut s = String::with_capacity(len);
    s.extend(tokens.iter().map(Token::as_str));
    s
}

