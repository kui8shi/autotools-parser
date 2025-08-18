//! The definition of a parser (and related methods) for the automake language.
use std::fmt::Debug;
use std::iter::empty as empty_iter;
use std::mem;
use std::str::FromStr;

use super::iter::{
    Multipeek, PeekableIterator, PositionIterator, TokenIter, TokenIterWrapper, TokenIterator,
    UnmatchedError,
};
use super::{
    CommandGroupDelimiters, ParseError, ParseErrorKind, ParseResult, Parser, ParserIterator,
    SourcePos, CASE, DO, DONE, ELIF, ELSE, ESAC, FI, FOR, FUNCTION, IF, IN, THEN, UNTIL, WHILE,
};
use crate::ast::am::{AmAssignOp, AmLine, AmVar, MakeDF, MakeParameter};
use crate::ast::builder::ConcatWordKind::{self, Concat, Single};
use crate::ast::builder::QuoteWordKind::{DoubleQuoted, Simple, SingleQuoted};
use crate::ast::builder::{self, AutomakeNodeBuilder, MakeBuilder, ShellBuilder, WordKind};
use crate::ast::node::{AcWord, Node, NodeId};
use crate::ast::{self, DefaultArithmetic, DefaultParameter};
use crate::token::Token;

/// A parser which will use a node-based AST builder implementation.
pub type AutomakeNodeParser<I> = AutomakeParser<I, AutomakeNodeBuilder<()>>;

use ParseErrorKind::*;
use Token::*;

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
}

impl<I, B> Parser for AutomakeParser<I, B>
where
    I: Iterator<Item = Token>,
    B: ShellBuilder + MakeBuilder,
    B::WordFragment: Into<Option<String>> + Clone + Debug,
{
    type TopLevel = B::Statement;
    type Error = B::Error;
    fn entry(&mut self) -> ParseResult<Option<Self::TopLevel>, Self::Error> {
        self.automake_statement()
    }
}

impl<I, B> IntoIterator for AutomakeParser<I, B>
where
    I: Iterator<Item = Token>,
    B: ShellBuilder + MakeBuilder,
    B::WordFragment: Into<Option<String>> + Clone + Debug,
{
    type IntoIter = ParserIterator<Self>;
    type Item = <Self::IntoIter as Iterator>::Item;

    fn into_iter(self) -> Self::IntoIter {
        ParserIterator::new(self)
    }
}

/// A parser for the shell language. It will parse shell commands from a
/// stream of shell `Token`s, and pass them to an AST builder.
#[derive(Debug)]
pub struct AutomakeParser<I, B> {
    iter: TokenIterWrapper<I>,
    builder: B,
    in_recipe: bool,
}

impl<I: Iterator<Item = Token>, B: ShellBuilder + MakeBuilder + Default> AutomakeParser<I, B>
where
    B::WordFragment: Into<Option<String>> + Clone + Debug,
{
    /// Creates a new Parser from a Token iterator or collection.
    pub fn new<T>(iter: T) -> AutomakeParser<I, B>
    where
        T: IntoIterator<Item = Token, IntoIter = I>,
    {
        AutomakeParser::with_builder(iter.into_iter(), Default::default())
    }

    /// Creates a new Parser with options
    pub fn new_with_config<T>(iter: T) -> AutomakeParser<I, B>
    where
        T: IntoIterator<Item = Token, IntoIter = I>,
    {
        AutomakeParser::with_builder(iter.into_iter(), Default::default())
    }
}

impl<I, U> AutomakeParser<I, AutomakeNodeBuilder<U>>
where
    I: Iterator<Item = Token>,
    <AutomakeNodeBuilder<U> as builder::BuilderBase>::WordFragment: From<ast::node::WordFragment<AcWord>>
        + Into<Option<ast::node::WordFragment<AcWord>>>
        + Into<Option<String>>
        + Clone,
    U: Default,
{
    /// Parse all complete commands
    /// (special method for NodeBuilder to easily take its state)
    pub fn parse_all(mut self) -> (slab::Slab<Node<AmLine, U>>, Vec<NodeId>) {
        let mut top_ids = Vec::new();
        // Parse all complete commands
        let mut take = || match self.automake_statement() {
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

impl<I, B> AutomakeParser<I, B>
where
    I: Iterator<Item = Token>,
    B: ShellBuilder + MakeBuilder,
    B::WordFragment: Into<Option<String>> + Clone + Debug,
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
    pub fn with_builder(iter: I, builder: B) -> Self {
        AutomakeParser {
            iter: TokenIterWrapper::Regular(TokenIter::new(iter)),
            builder,
            in_recipe: false,
        }
    }

    /// Returns the parser's current position in the source.
    pub fn pos(&self) -> SourcePos {
        self.iter.pos()
    }

    /// Parses a single automake top-level statement.
    pub fn automake_conditional(&mut self) -> ParseResult<B::Statement, B::Error> {
        const ENDIF: &str = "endif";
        self.skip_whitespace();
        let guard_var = if let Some(Name(name)) = self.iter.next() {
            name.to_owned()
        } else {
            return Err(self.make_unexpected_err());
        };
        self.linebreak_preserve_line_head_whitespace();
        let mut after_else_keyword = false;
        let mut then = Vec::new();
        let mut otherwise = Vec::new();
        loop {
            if let Some(directive) = self.peek_reserved_word(&[ELSE, ENDIF]) {
                if directive == ELSE {
                    after_else_keyword = true;
                    self.newline();
                } else if directive == ENDIF {
                    self.word()?;
                    self.newline();
                    break;
                }
            }
            let stmt = if self.in_recipe {
                self.automake_recipe()?.unwrap()
            } else {
                self.automake_statement()?.unwrap()
            };
            match after_else_keyword {
                true => then.push(stmt),
                false => otherwise.push(stmt),
            }
        }
        Ok(self.builder.conditional(guard_var, then, otherwise)?)
    }

    /// Parses a single automake rule statement.
    pub fn automake_rule(&mut self) -> ParseResult<Option<B::Statement>, B::Error> {
        let target = self.words_with_delim(&[Colon])?;
        eat!(self, { Colon => {} });
        self.skip_whitespace();
        let dependency = self.words_with_delim(&[Newline])?;
        eat!(self, { Newline => {} });
        let mut recipes = Vec::new();
        while let Some(recipe) = self.automake_recipe()? {
            recipes.push(recipe);
        }
        Ok(Some(self.builder.rule(target, dependency, recipes)?))
    }

    /// Parses a single automake assignment statement.
    pub fn automake_assignment(&mut self) -> ParseResult<Option<B::Statement>, B::Error> {
        if let Some((name, op)) = self.automake_assign_op()? {
            // assignment statement
            self.skip_whitespace();
            let words = self.words_with_delim(&[Newline])?;
            eat!(self, { Newline => {} });
            Ok(Some(self.builder.assignment(name, op, words)?))
        } else {
            Ok(None)
        }
    }

    /// Parses a single automake shell command statement.
    pub fn automake_command(&mut self) -> ParseResult<Option<B::Statement>, B::Error> {
        eat_maybe!(self, { At => {} });
        if let Some(cmd) = self.complete_command()? {
            Ok(Some(self.builder.shell_as_recipe(cmd)?))
        } else {
            Ok(None)
        }
    }

    /// Parses a single automake recipe command.
    pub fn automake_recipe(&mut self) -> ParseResult<Option<B::Statement>, B::Error> {
        let start_pos = self.iter.pos();
        let pre_stmt_comments = self.linebreak_preserve_line_head_whitespace();

        match self.iter.peek() {
            Some(Name(s)) => {
                if s == IF {
                    Ok(Some(self.automake_conditional()?))
                } else {
                    // now we are out of recipe.
                    Ok(None)
                }
            }
            Some(Whitespace(s)) => {
                if s.starts_with("\t") {
                    let c = self.automake_command();
                    c
                } else {
                    Err(self.make_unexpected_err())
                }
            }
            _ => Ok(None),
        }
    }

    /// Parses a single automake top-level statement.
    pub fn automake_statement(&mut self) -> ParseResult<Option<B::Statement>, B::Error> {
        const INCLUDE: &str = "include";
        let start_pos = self.iter.pos();
        let pre_stmt_comments = self.linebreak_preserve_line_head_whitespace();

        match self.iter.peek().cloned() {
            Some(Name(s)) => {
                if s == IF {
                    // conditional statement
                    eat!(self, { Name(_) => {} });
                    Ok(Some(self.automake_conditional()?))
                } else if s == INCLUDE {
                    // include statement
                    eat!(self, { Name(_) => {} });
                    let path = self.word()?.unwrap();
                    Ok(Some(self.builder.include(path)?))
                } else {
                    if let Some(assignment) = self.automake_assignment()? {
                        Ok(Some(assignment))
                    } else {
                        Ok(self.automake_rule()?)
                    }
                }
            }
            Some(Dollar | Percent | Dot) => Ok(self.automake_rule()?),
            Some(Whitespace(s)) => Err(self.make_unexpected_err()),
            None => Ok(None),
            _ => Err(self.make_unexpected_err()),
        }
    }

    /// Parse an automake assignment operator without consuming tokens until confirmed.
    pub fn automake_assign_op(&mut self) -> ParseResult<Option<(String, AmAssignOp)>, B::Error> {
        use AmAssignOp::*;
        let may_op = {
            let mut peeked = self.iter.multipeek();
            peeked.peek_next(); // eat identifier
            let first_token_after_whitespaces = Self::multipeek_skip_whitespace(&mut peeked);
            match first_token_after_whitespaces {
                Some(Equals) => Some(Lazy),
                Some(Colon) if peeked.peek_next() == Some(&Equals) => Some(Instant),
                Some(Plus) if peeked.peek_next() == Some(&Equals) => Some(Append),
                _ => None,
            }
        };
        if let Some(op) = may_op {
            if let Some(Name(ref name)) = self.iter.next() {
                self.skip_whitespace();
                match &op {
                    Lazy => {
                        eat!(self, { Equals => {} });
                    }
                    Instant => {
                        eat!(self, { Colon => {} });
                        eat!(self, { Equals => {} });
                    }
                    Append => {
                        eat!(self, { Plus => {} });
                        eat!(self, { Equals => {} });
                    }
                };
                Ok(Some((name.to_owned(), op)))
            } else {
                Err(self.make_unexpected_err())
            }
        } else {
            Ok(None)
        }
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
                true,
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
        pre_cmd_comments: Vec<builder::Newline>,
        start_pos: SourcePos,
        preserve_line_head: bool,
    ) -> ParseResult<B::Command, B::Error> {
        let cmd = self.and_or_list()?;

        let (sep, cmd_comment) = eat_maybe!(self, {
            Semi => {
                (builder::SeparatorKind::Semi, if preserve_line_head { self.newline_dumb() } else {self.newline() } )
            },
            Amp  => {
                (builder::SeparatorKind::Amp , if preserve_line_head { self.newline_dumb() } else {self.newline() } )
            };
            _ => {
                match if preserve_line_head { self.newline_dumb() } else { self.newline() } {
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

        let mut vars = Vec::new();
        let mut cmd_args = Vec::new();

        // 1st loop: [EnvVar | Redirect]* + CmdWord(exec)
        loop {
            self.skip_whitespace();
            let is_name = {
                let mut peeked = self.iter.multipeek();
                if let Some(&Name(_)) = peeked.peek_next() {
                    let maybe_equal = peeked.peek_next();
                    Some(&Equals) == maybe_equal
                } else {
                    false
                }
            };

            if is_name {
                if let Some(Name(var)) = self.iter.next() {
                    self.iter.next(); // Consume the =

                    let value = if let Some(&Whitespace(_)) = self.iter.peek() {
                        None
                    } else {
                        let w = match self.word_preserve_trailing_whitespace_raw()? {
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

        let (src_fd, src_fd_as_word) = match self.word_preserve_trailing_whitespace_raw()? {
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
                match $parser.word_preserve_trailing_whitespace_raw()? {
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
                    let path = if let Some(p) = $parser.word_preserve_trailing_whitespace_raw()? {
                        p
                    } else {
                        return Err($parser.make_unexpected_err());
                    };
                    path
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
                $result.map_err(|e: UnmatchedError| ParseError::new(Unmatched(e.0, e.1)))?
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
        self.word_preserve_trailing_whitespace_raw_with_delim(None)
    }

    /// Parses words until a specified word delimiter hits
    fn words_with_delim(&mut self, delims: &[Token]) -> ParseResult<Vec<B::Word>, B::Error> {
        let mut ret = Vec::new();
        while let Some(w) = self.word_preserve_trailing_whitespace_raw_with_delim(Some(delims))? {
            let word = self.builder.word(w)?;
            ret.push(word);
            if let Some(tok) = self.iter.peek() {
                if delims.iter().any(|t| t == tok) {
                    break;
                }
            }
            self.skip_whitespace();
        }
        Ok(ret)
    }

    /// Identical to `Parser::word_preserve_trailing_whitespace_raw()` but
    /// allows for specifying an arbitrary token as a word delimiter.
    fn word_preserve_trailing_whitespace_raw_with_delim(
        &mut self,
        delims: Option<&[Token]>,
    ) -> ParseResult<Option<ConcatWordKind<B::WordFragment>>, B::Error> {
        self.skip_whitespace();
        // Make sure we don't consume comments,
        // e.g. if a # is at the start of a word.
        match self.iter.peek() {
            Some(&Pound) => return Ok(None),
            Some(Name(n)) if n == "dnl" => return Ok(None),
            _ => (),
        }
        let mut words = Vec::new();
        loop {
            if let Some(delims) = delims {
                if let Some(tok) = self.iter.peek() {
                    if delims.iter().any(|t| t == tok) {
                        break;
                    }
                }
            }

            match self.iter.peek() {
                Some(&CurlyOpen) | Some(&CurlyClose) | Some(&SquareOpen) | Some(&SquareClose)
                | Some(&SingleQuote) | Some(&DoubleQuote) | Some(&Pound) | Some(&Star)
                | Some(&Question) | Some(&Tilde) | Some(&Bang) | Some(&Backslash)
                | Some(&Percent) | Some(&Dash) | Some(&Equals) | Some(&Plus) | Some(&Colon)
                | Some(&Caret) | Some(&Slash) | Some(&Comma) | Some(&Dot) | Some(&Name(_))
                | Some(&Literal(_)) => {}

                Some(&Backtick) => {
                    let backtick = self.backticked_raw()?;
                    words.push(Simple(self.builder.word_fragment(backtick)?));
                    continue;
                }

                Some(&At) => {
                    let template = self.automake_template()?;
                    words.push(Simple(template));
                    continue;
                }

                Some(&Dollar) | Some(&ParamPositional(_)) => {
                    let param = self.parameter()?;
                    words.push(Simple(param));
                    continue;
                }

                Some(&EmptyQuotes) => {
                    // Empty quotes work like an empty string while being a delimiter of tokens.
                    self.iter.next();
                    continue;
                }

                Some(&Newline) | Some(&ParenOpen) | Some(&ParenClose) | Some(&Semi)
                | Some(&Amp) | Some(&Pipe) | Some(&AndIf) | Some(&OrIf) | Some(&DSemi)
                | Some(&Less) | Some(&Great) | Some(&DLess) | Some(&DGreat) | Some(&GreatAnd)
                | Some(&LessAnd) | Some(&DLessDash) | Some(&Clobber) | Some(&LessGreat)
                | Some(&Whitespace(_)) | None => break,
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
        let is_delim = |tok: Option<Token>| {
            if let Some(delims) = delims {
                delims.iter().find(|&d| Some(d) == tok.as_ref()).is_some()
            } else {
                tok.is_none()
            }
        };
        loop {
            if is_delim(self.iter.peek().cloned()) {
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

            // Make sure we don't consume any $ (or any specific parameter token)
            // we find since the `parameter` method expects to consume them.
            match self.iter.peek() {
                Some(&Dollar) | Some(&ParamPositional(_)) => {
                    let param = self.parameter()?;
                    store!(param);
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

    /// Parses parameters such as `$$`, `$1`, `$foo`, etc, or
    /// parameter substitutions such as `$(cmd)`, `${param-word}`, etc.
    ///
    /// Since it is possible that a leading `$` is not followed by a valid
    /// parameter, the `$` should be treated as a literal. Thus this method
    /// returns an `Word`, which will capture both cases where a literal or
    /// parameter is parsed.
    pub fn parameter(&mut self) -> ParseResult<B::WordFragment, B::Error> {
        eat!(self, { Dollar => {} });
        let ret = match self.iter.peek() {
            Some(Dollar) => {
                // shell parameter
                let p = self.shell_parameter()?;
                self.builder.word_fragment(p)?
            }
            Some(&ParenOpen) => {
                // automake variable
                eat!(self, { ParenOpen => {} });
                let p = self.automake_parameter()?;
                eat!(self, { ParenClose => {} });
                self.builder.variable(AmVar::Param(p))?
            }
            Some(_) => {
                let p = self.automake_parameter()?;
                self.builder.variable(AmVar::Param(p))?
            }
            _ => return Err(self.make_unexpected_err()),
        };
        Ok(ret)
    }

    /// Parses escaped shell parameters e.g. $$var, $$(subst).
    fn shell_parameter(&mut self) -> ParseResult<WordKind<B::Command, B::WordFragment>, B::Error> {
        use crate::ast::Parameter;

        let start_pos = self.iter.pos();
        let ret = match self.iter.next() {
            Some(ParamPositional(p)) => Ok(WordKind::Param(Parameter::Positional(p as u32))),
            Some(Dollar) => {
                let p = match self.iter.peek() {
                    Some(&Star) | Some(&Pound) | Some(&Question) | Some(&Dollar) | Some(&Bang)
                    | Some(&Dash) | Some(&At) | Some(&Name(_)) => {
                        Ok(WordKind::Param(self.shell_parameter_inner()?))
                    }

                    Some(&CurlyOpen) => self.parameter_substitution_raw(),

                    Some(Literal(s)) if s.len() == 1 && s.chars().last().unwrap().is_numeric() => {
                        // quoted positional parameter (e.g. $[1])
                        let num = s.parse::<u32>().unwrap();
                        self.iter.next();
                        Ok(WordKind::Param(Parameter::Positional(num)))
                    }

                    _ => Ok(WordKind::Literal(Dollar.to_string())),
                };
                p
            }
            Some(t) => Err(ParseError::new(Unexpected(t, start_pos))),
            None => Err(ParseError::new(UnexpectedEOF)),
        };
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

            match self.word_preserve_trailing_whitespace_raw_with_delim(Some(&[CurlyClose]))? {
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

                let param = self.shell_parameter_inner()?;
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
                        let param = self.shell_parameter_inner()?;
                        eat!(self, { CurlyClose => { Len(param) } })
                    }

                    _ => return self.parameter_substitution_body_raw(param, curly_open_pos),
                };

                Ok(WordKind::Subst(Box::new(subst)))
            }

            _ => Err(self.make_unexpected_err()),
        }
    }

    fn make_parameter_extension(&mut self) -> Option<MakeDF> {
        let df = if let Some(&Name(ref s)) = self.iter.peek() {
            if s == "D" {
                Some(MakeDF::Dir)
            } else if s == "F" {
                Some(MakeDF::File)
            } else {
                None
            }
        } else {
            None
        };
        if df.is_some() {
            self.iter.next();
        }
        df
    }

    fn automake_parameter(&mut self) -> ParseResult<MakeParameter, B::Error> {
        use MakeParameter::*;
        let start_pos = self.iter.pos();

        let param = match self.iter.next() {
            Some(At) => Target(self.make_parameter_extension()),
            Some(Star) => Match(self.make_parameter_extension()),
            Some(Less) => FirstDependency(self.make_parameter_extension()),
            Some(Caret) => AllDependency(self.make_parameter_extension()),
            Some(Plus) => AllDependencyAllowingDuplicate(self.make_parameter_extension()),
            Some(Question) => NewerDependency(self.make_parameter_extension()),
            Some(Name(n)) => Var(n),
            Some(t) => return Err(ParseError::new(BadSubst(t, start_pos))),
            None => return Err(ParseError::new(UnexpectedEOF)),
        };

        Ok(param)
    }

    fn automake_template(&mut self) -> ParseResult<B::WordFragment, B::Error> {
        let is_template = {
            let mut peeked = self.iter.multipeek();
            if !matches!(peeked.peek_next(), Some(Name(_) | Literal(_))) {
                false
            } else if !matches!(peeked.peek_next(), Some(At)) {
                false
            } else {
                true
            }
        };

        if is_template {
            let name = match self.iter.next() {
                Some(Name(s)) => s.to_owned(),
                Some(Literal(s)) => s.to_owned(),
                _ => unreachable!(),
            };
            eat!(self, { At => { }});
            Ok(self.builder.variable(AmVar::Template(name))?)
        } else {
            eat!(self, { At => { }});
            Ok(self
                .builder
                .word_fragment(WordKind::Literal("@".to_owned()))?)
        }
    }

    /// Parses a valid parameter that can appear inside a set of curly braces.
    fn shell_parameter_inner(&mut self) -> ParseResult<DefaultParameter, B::Error> {
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
        let cmd = match kw.or_else(|| self.next_compound_command_type()) {
            Some(CompoundCmdKeyword::If) => {
                let fragments = self.if_command()?;
                let io = self.redirect_list()?;
                self.builder.if_command(fragments, io)?
            }

            Some(CompoundCmdKeyword::While) | Some(CompoundCmdKeyword::Until) => {
                let (until, guard_body_pair) = self.loop_command()?;
                let io = self.redirect_list()?;
                self.builder.loop_command(until, guard_body_pair, io)?
            }

            Some(CompoundCmdKeyword::For) => {
                let for_fragments = self.for_command()?;
                let io = self.redirect_list()?;
                self.builder.for_command(for_fragments, io)?
            }

            Some(CompoundCmdKeyword::Case) => {
                let fragments = self.case_command()?;
                let io = self.redirect_list()?;
                self.builder.case_command(fragments, io)?
            }

            Some(CompoundCmdKeyword::Brace) => {
                let cmds = self.brace_group()?;
                let io = self.redirect_list()?;
                self.builder.brace_group(cmds, io)?
            }

            Some(CompoundCmdKeyword::Subshell) => {
                let cmds = self.subshell()?;
                let io = self.redirect_list()?;
                self.builder.subshell(cmds, io)?
            }

            None => return Err(self.make_unexpected_err()),
        };

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

        let word = match self.word_preserve_trailing_whitespace_raw_with_delim(None)? {
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

            let mut patterns = Vec::new();
            loop {
                // only see the outermost opening quote.
                match self.word()? {
                    Some(p) => patterns.push(p),
                    None => return Err(self.make_unexpected_err()),
                }
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
                loop {
                    match peeked.peek_next() {
                        Some(&Whitespace(_)) => continue,
                        Some(&ParenOpen) => break true,
                        _ => break false,
                    }
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

    /// Parses zero or more `Token::Newline`s, skipping trailing whitespace but capturing comments.
    /// @kui8shi
    /// Since automake's syntax really cares about the first token in a line, we duplicated
    /// `linebreak` to ensure that callers of this always start parsing from the head of line.
    #[inline]
    pub fn linebreak_preserve_line_head_whitespace(&mut self) -> Vec<builder::Newline> {
        let mut lines = Vec::new();
        while self.is_newline() {
            lines.push(self.newline().unwrap());
        }
        lines
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

    /// Dumb version of `newline`
    pub fn newline_dumb(&mut self) -> Option<builder::Newline> {
        loop {
            match self.iter.peek() {
                Some(&Pound) => {
                    let comment = self
                        .iter
                        .by_ref()
                        .take_while(|t| t != &Newline)
                        .collect::<Vec<_>>();
                    break Some(builder::Newline(Some(concat_tokens(&comment))));
                }

                Some(&Newline) => {
                    self.iter.next();
                    break Some(builder::Newline(None));
                }

                Some(_) => {
                    self.iter.next();
                    continue;
                }

                None => break None,
            }
        }
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

            Some(&Newline) => {
                self.iter.next();
                Some(builder::Newline(None))
            }

            _ => None,
        }
    }

    /// Check if a newline is coming without consuming tokens.
    pub fn is_newline(&mut self) -> bool {
        let mut peeked = self.iter.multipeek();
        let first_token_after_whitespaces = Self::multipeek_skip_whitespace(&mut peeked);
        match first_token_after_whitespaces {
            Some(Pound) | Some(Newline) => true,
            _ => false,
        }
    }

    fn multipeek_skip_whitespace(peeked: &mut Multipeek<'_>) -> Option<Token> {
        let mut backslash = false;
        loop {
            match peeked.peek_next() {
                Some(Whitespace(_)) => continue,
                Some(Backslash) => backslash = true,
                Some(Newline) if backslash => continue,
                s => {
                    if backslash {
                        break None;
                    } else {
                        break s.cloned();
                    }
                }
            }
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
        let group = self.command_group_internal(cfg)?;
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
        let found_delim = |slf: &mut AutomakeParser<_, _>| {
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
            let start_pos = self.iter.pos();

            let leading_comments = self.linebreak();

            if found_delim(self) || self.iter.peek().is_none() {
                debug_assert!(trailing_comments.is_empty());
                trailing_comments = leading_comments;
                break;
            }

            cmds.push(self.complete_command_with_leading_comments(
                leading_comments,
                start_pos,
                false,
            )?);
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
