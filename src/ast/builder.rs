//! Defines an interfaces to receive parse data and construct ASTs.
//!
//! This allows the parser to remain agnostic of the required source
//! representation, and frees up the library user to substitute their own.
//! If one does not require a custom AST representation, this module offers
//! a reasonable default builder implementation.
//!
//! If a custom AST representation is required you will need to implement
//! the `Builder` trait for your AST. Otherwise you can provide the `DefaultBuilder`
//! struct to the parser if you wish to use the default AST implementation.

use std::fmt::Display;

use crate::ast::{
    AndOr, DefaultArithmetic, DefaultParameter, M4Argument, RedirectOrCmdWord, RedirectOrEnvVar,
};
use crate::m4_macro::SideEffect;

mod default_builder;
mod minimal_builder;
mod node_builder;

pub use self::default_builder::*;
pub use self::minimal_builder::MinimalBuilder;
pub use self::node_builder::AutoconfNodeBuilder;
pub use self::node_builder::AutomakeNodeBuilder;

use super::am::{AmAssignOp, AmVar};

/// An indicator to the builder of how complete commands are separated.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum SeparatorKind {
    /// A semicolon appears between commands, normally indicating a sequence.
    Semi,
    /// An ampersand appears between commands, normally indicating an asyncronous job.
    Amp,
    /// A newline (and possibly a comment) appears at the end of a command before the next.
    Newline,
    /// The command was delimited by a token (e.g. a compound command delimiter) or
    /// the end of input, but is *not* followed by another sequential command.
    Other,
}

/// An indicator to the builder whether a `while` or `until` command was parsed.
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum LoopKind {
    /// A `while` command was parsed, normally indicating the loop's body should be run
    /// while the guard's exit status is successful.
    While,
    /// An `until` command was parsed, normally indicating the loop's body should be run
    /// until the guard's exit status becomes successful.
    Until,
}

/// A grouping of a list of commands and any comments trailing after the commands.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CommandGroup<C> {
    /// The sequential list of commands.
    pub commands: Vec<C>,
    /// Any trailing comments appearing on the next line after the last command.
    pub trailing_comments: Vec<Newline>,
}

/// A grouping of guard and body commands, and any comments they may have.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct GuardBodyPairGroup<C> {
    /// The guard commands, which if successful, should lead to the
    /// execution of the body commands.
    pub guard: CommandGroup<C>,
    /// The body commands to execute if the guard is successful.
    pub body: CommandGroup<C>,
}

/// Parsed fragments relating to a shell `if` command.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct IfFragments<C> {
    /// A list of conditionals branches.
    pub conditionals: Vec<GuardBodyPairGroup<C>>,
    /// The `else` branch, if any,
    pub else_branch: Option<CommandGroup<C>>,
}

/// Parsed fragments relating to a shell `for` command.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ForFragments<C, W> {
    /// The name of the variable to which each of the words will be bound.
    pub var: String,
    /// A comment that begins on the same line as the variable declaration.
    pub var_comment: Option<Newline>,
    /// Any comments after the variable declaration, a group of words to
    /// iterator over, and comment defined on the same line as the words.
    pub words: Option<(Vec<Newline>, Vec<W>, Option<Newline>)>,
    /// Any comments that appear after the `words` declaration (if it exists),
    /// but before the body of commands.
    pub pre_body_comments: Vec<Newline>,
    /// The body to be invoked for every iteration.
    pub body: CommandGroup<C>,
}

/// Parsed fragments relating to a shell `case` command.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CaseFragments<C, W> {
    /// The word to be matched against.
    pub word: W,
    /// The comments appearing after the word to match but before the `in` reserved word.
    pub post_word_comments: Vec<Newline>,
    /// A comment appearing immediately after the `in` reserved word,
    /// yet still on the same line.
    pub in_comment: Option<Newline>,
    /// All the possible branches of the `case` command.
    pub arms: Vec<CaseArm<C, W>>,
    /// The comments appearing after the last arm but before the `esac` reserved word.
    pub post_arms_comments: Vec<Newline>,
}

/// An individual "unit of execution" within a `case` command.
///
/// Each arm has a number of pattern alternatives, and a body
/// of commands to run if any pattern matches.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CaseArm<C, W> {
    /// The patterns which correspond to this case arm.
    pub patterns: CasePatternFragments<W>,
    /// The body of commands to run if any pattern matches.
    pub body: CommandGroup<C>,
    /// A comment appearing at the end of the arm declaration,
    /// i.e. after `;;` but on the same line.
    pub arm_comment: Option<Newline>,
}

/// Parsed fragments relating to patterns in a shell `case` command.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CasePatternFragments<W> {
    /// Comments appearing after a previous arm, but before the start of a pattern.
    pub pre_pattern_comments: Vec<Newline>,
    /// Pattern alternatives which all correspond to the same case arm.
    pub pattern_alternatives: Vec<W>,
    /// A comment appearing at the end of the pattern declaration on the same line.
    pub pattern_comment: Option<Newline>,
}

/// An indicator to the builder what kind of complex word was parsed.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ConcatWordKind<X> {
    /// Several distinct words concatenated together.
    Concat(Vec<QuoteWordKind<X>>),
    /// A regular word.
    Single(QuoteWordKind<X>),
}

/// An indicator to the builder what kind of word was parsed.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum QuoteWordKind<X> {
    /// A regular word.
    Simple(X),
    /// List of words concatenated within double quotes.
    DoubleQuoted(Vec<X>),
    /// List of words concatenated within single quotes. Virtually
    /// identical as a literal, but makes a distinction between the two.
    // @kui8shi
    /// We do not parse m4 macro calls inside the single quoted string.
    SingleQuoted(String),
}

/// An indicator to the builder what kind of simple word was parsed.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum WordKind<C, W> {
    /// A non-special literal word.
    Literal(String),
    /// Access of a value inside a parameter, e.g. `$foo` or `$$`.
    Param(DefaultParameter),
    /// A parameter substitution, e.g. `${param-word}`.
    Subst(Box<ParameterSubstitutionKind<C, ConcatWordKind<W>>>),
    /// Represents the standard output of some command, e.g. \`echo foo\`.
    CommandSubst(CommandGroup<C>),
    /// A token which normally has a special meaning is treated as a literal
    /// because it was escaped, typically with a backslash, e.g. `\"`.
    Escaped(String),
    /// Represents `*`, useful for handling pattern expansions.
    Star,
    /// Represents `?`, useful for handling pattern expansions.
    Question,
    /// Represents `[`, useful for handling pattern expansions.
    SquareOpen,
    /// Represents `]`, useful for handling pattern expansions.
    SquareClose,
    /// Represents `~`, useful for handling tilde expansions.
    Tilde,
    /// Represents `:`, useful for handling tilde expansions.
    Colon,
}

/// Represents redirecting a command's file descriptors.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum RedirectKind<W> {
    /// Open a file for reading, e.g. `[n]< file`.
    Read(Option<u16>, W),
    /// Open a file for writing after truncating, e.g. `[n]> file`.
    Write(Option<u16>, W),
    /// Open a file for reading and writing, e.g. `[n]<> file`.
    ReadWrite(Option<u16>, W),
    /// Open a file for writing, appending to the end, e.g. `[n]>> file`.
    Append(Option<u16>, W),
    /// Open a file for writing, failing if the `noclobber` shell option is set, e.g. `[n]>| file`.
    Clobber(Option<u16>, W),
    /// Lines contained in the source that should be provided by as input to a file descriptor.
    Heredoc(Option<u16>, W),
    /// Duplicate a file descriptor for reading, e.g. `[n]<& [n|-]`.
    DupRead(Option<u16>, W),
    /// Duplicate a file descriptor for writing, e.g. `[n]>& [n|-]`.
    DupWrite(Option<u16>, W),
}

/// Represents the type of parameter that was parsed
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ParameterSubstitutionKind<C, W> {
    /// Returns the standard output of running a command, e.g. `$(cmd)`
    Command(CommandGroup<C>),
    /// Returns the length of the value of a parameter, e.g. ${#param}
    Len(DefaultParameter),
    /// Returns the resulting value of an arithmetic subsitution, e.g. `$(( x++ ))`
    Arith(Option<DefaultArithmetic>),
    /// Use a provided value if the parameter is null or unset, e.g.
    /// `${param:-[word]}`.
    /// The boolean indicates the presence of a `:`, and that if the parameter has
    /// a null value, that situation should be treated as if the parameter is unset.
    Default(bool, DefaultParameter, Option<W>),
    /// Assign a provided value to the parameter if it is null or unset,
    /// e.g. `${param:=[word]}`.
    /// The boolean indicates the presence of a `:`, and that if the parameter has
    /// a null value, that situation should be treated as if the parameter is unset.
    Assign(bool, DefaultParameter, Option<W>),
    /// If the parameter is null or unset, an error should result with the provided
    /// message, e.g. `${param:?[word]}`.
    /// The boolean indicates the presence of a `:`, and that if the parameter has
    /// a null value, that situation should be treated as if the parameter is unset.
    Error(bool, DefaultParameter, Option<W>),
    /// If the parameter is NOT null or unset, a provided word will be used,
    /// e.g. `${param:+[word]}`.
    /// The boolean indicates the presence of a `:`, and that if the parameter has
    /// a null value, that situation should be treated as if the parameter is unset.
    Alternative(bool, DefaultParameter, Option<W>),
    /// Remove smallest suffix pattern, e.g. `${param%pattern}`
    RemoveSmallestSuffix(DefaultParameter, Option<W>),
    /// Remove largest suffix pattern, e.g. `${param%%pattern}`
    RemoveLargestSuffix(DefaultParameter, Option<W>),
    /// Remove smallest prefix pattern, e.g. `${param#pattern}`
    RemoveSmallestPrefix(DefaultParameter, Option<W>),
    /// Remove largest prefix pattern, e.g. `${param##pattern}`
    RemoveLargestPrefix(DefaultParameter, Option<W>),
}

/// Represents a parsed newline, more specifically, the presense of a comment
/// immediately preceeding the newline.
///
/// Since shell comments are usually treated as a newline, they can be present
/// anywhere a newline can be as well. Thus if it is desired to retain comments
/// they can be optionally attached to a parsed newline.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Newline(pub Option<String>);

/// Basis trait of ~Builder
pub trait BuilderBase {
    /// The type which represents a complete, top-level command.
    type Command;
    /// The type which represents compound commands like `if`, `case`, `for`, etc.
    type CompoundCommand;
    /// The type which represents shell words, which can be command names or arguments.
    type Word;
    /// The type which represents simplest shell words.
    type WordFragment;
    /// The type which represents a file descriptor redirection.
    type Redirect;
    /// A type for returning custom parse/build errors.
    type Error;
}

/// A trait which defines an interface which the parser defined in the `parse` module
/// uses to delegate Abstract Syntax Tree creation. The methods defined here correspond
/// to their respectively named methods on the parser, and accept the relevant data for
/// each shell command type.
pub trait ShellBuilder: BuilderBase {
    /// The type which represents an and/or list of commands.
    type CommandList;
    /// The type which represents a command that can be used in an and/or command list.
    type ListableCommand;
    /// The type which represents a command that can be used in a pipeline.
    type PipeableCommand;

    /// Invoked once a complete command is found. That is, a command delimited by a
    /// newline, semicolon, ampersand, or the end of input.
    ///
    /// # Arguments
    /// * pre_cmd_comments: any comments that appear before the start of the command
    /// * list: an and/or list of commands previously generated by the same builder
    /// * separator: indicates how the command was delimited
    /// * cmd_comment: a comment that appears at the end of the command
    fn complete_command(
        &mut self,
        pre_cmd_comments: Vec<Newline>,
        list: Self::CommandList,
        separator: SeparatorKind,
        cmd_comment: Option<Newline>,
        range: (usize, usize),
    ) -> Result<Self::Command, Self::Error>;

    /// Invoked when multiple commands are parsed which are separated by `&&` or `||`.
    /// Typically after the first command is run, each of the following commands may or
    /// may not be executed, depending on the exit status of the previously executed command.
    ///
    /// # Arguments
    /// * first: the first command before any `&&` or `||` separator
    /// * rest: A collection of comments after the last separator and the next command.
    fn and_or_list(
        &mut self,
        first: Self::ListableCommand,
        rest: Vec<(Vec<Newline>, AndOr<Self::ListableCommand>)>,
    ) -> Result<Self::CommandList, Self::Error>;

    /// Invoked when a pipeline of commands is parsed.
    /// A pipeline is one or more commands where the standard output of the previous
    /// typically becomes the standard input of the next.
    ///
    /// # Arguments
    /// * bang: the presence of a `!` at the start of the pipeline, typically indicating
    /// that the pipeline's exit status should be logically inverted.
    /// * cmds: a collection of tuples which are any comments appearing after a pipe token, followed
    /// by the command itself, all in the order they were parsed
    fn pipeline(
        &mut self,
        bang: bool,
        cmds: Vec<(Vec<Newline>, Self::PipeableCommand)>,
    ) -> Result<Self::ListableCommand, Self::Error>;

    /// Invoked when the "simplest" possible command is parsed: an executable with arguments.
    ///
    /// # Arguments
    /// * redirects_or_env_vars: redirections or environment variables that occur before any command
    /// * redirects_or_cmd_words: redirections or any command or argument
    fn simple_command(
        &mut self,
        redirects_or_env_vars: Vec<RedirectOrEnvVar<Self::Redirect, String, Self::Word>>,
        redirects_or_cmd_words: Vec<RedirectOrCmdWord<Self::Redirect, Self::Word>>,
    ) -> Result<Self::PipeableCommand, Self::Error>;

    /// Invoked when a non-zero number of commands were parsed between balanced curly braces.
    /// Typically these commands should run within the current shell environment.
    ///
    /// # Arguments
    /// * cmds: the commands that were parsed between braces
    /// * redirects: any redirects to be applied over the **entire** group of commands
    fn brace_group(
        &mut self,
        cmds: CommandGroup<Self::Command>,
        redirects: Vec<Self::Redirect>,
    ) -> Result<Self::CompoundCommand, Self::Error>;

    /// Invoked when a non-zero number of commands were parsed between balanced parentheses.
    /// Typically these commands should run within their own environment without affecting
    /// the shell's global environment.
    ///
    /// # Arguments
    /// * cmds: the commands that were parsed between parens
    /// * redirects: any redirects to be applied over the **entire** group of commands
    fn subshell(
        &mut self,
        cmds: CommandGroup<Self::Command>,
        redirects: Vec<Self::Redirect>,
    ) -> Result<Self::CompoundCommand, Self::Error>;

    /// Invoked when a loop command like `while` or `until` is parsed.
    /// Typically these commands will execute their body based on the exit status of their guard.
    ///
    /// # Arguments
    /// * kind: the type of the loop: `while` or `until`
    /// * guard: commands that determine how long the loop will run for
    /// * body: commands to be run every iteration of the loop
    /// * redirects: any redirects to be applied over **all** commands part of the loop
    fn loop_command(
        &mut self,
        kind: LoopKind,
        guard_body_pair: GuardBodyPairGroup<Self::Command>,
        redirects: Vec<Self::Redirect>,
    ) -> Result<Self::CompoundCommand, Self::Error>;

    /// Invoked when an `if` conditional command is parsed.
    /// Typically an `if` command is made up of one or more guard-body pairs, where the body
    /// of the first successful corresponding guard is executed. There can also be an optional
    /// `else` part to be run if no guard is successful.
    ///
    /// # Arguments
    /// * fragments: parsed fragments relating to a shell `if` command.
    /// * redirects: any redirects to be applied over **all** commands within the `if` command
    fn if_command(
        &mut self,
        fragments: IfFragments<Self::Command>,
        redirects: Vec<Self::Redirect>,
    ) -> Result<Self::CompoundCommand, Self::Error>;

    /// Invoked when a `for` command is parsed.
    /// Typically a `for` command binds a variable to each member in a group of words and
    /// invokes its body with that variable present in the environment. If no words are
    /// specified, the command will iterate over the arguments to the script or enclosing function.
    ///
    /// # Arguments
    /// * fragments: parsed fragments relating to a shell `for` command.
    /// * redirects: any redirects to be applied over **all** commands within the `for` command
    fn for_command(
        &mut self,
        fragments: ForFragments<Self::Command, Self::Word>,
        redirects: Vec<Self::Redirect>,
    ) -> Result<Self::CompoundCommand, Self::Error>;

    /// Invoked when a `case` command is parsed.
    /// Typically this command will execute certain commands when a given word matches a pattern.
    ///
    /// # Arguments
    /// * fragments: parsed fragments relating to a shell `case` command.
    /// * redirects: any redirects to be applied over **all** commands part of the `case` block
    fn case_command(
        &mut self,
        fragments: CaseFragments<Self::Command, Self::Word>,
        redirects: Vec<Self::Redirect>,
    ) -> Result<Self::CompoundCommand, Self::Error>;

    /// Bridges the gap between a `PipeableCommand` and a `CompoundCommand` since
    /// `CompoundCommand`s are typically `PipeableCommand`s as well.
    ///
    /// # Arguments
    /// cmd: The `CompoundCommand` to convert into a `PipeableCommand`
    fn compound_command_into_pipeable(
        &mut self,
        cmd: Self::CompoundCommand,
    ) -> Result<Self::PipeableCommand, Self::Error>;

    /// Invoked when a function declaration is parsed.
    /// Typically a function declaration overwrites any previously defined function
    /// within the current environment.
    ///
    /// # Arguments
    /// * name: the name of the function to be created
    /// * post_name_comments: any comments appearing after the function name but before the body
    /// * body: commands to be run when the function is invoked
    fn function_declaration(
        &mut self,
        name: String,
        post_name_comments: Vec<Newline>,
        body: Self::CompoundCommand,
    ) -> Result<Self::PipeableCommand, Self::Error>;

    /// Invoked when only comments are parsed with no commands following.
    /// This can occur if an entire shell script is commented out or if there
    /// are comments present at the end of the script.
    ///
    /// # Arguments
    /// * comments: the parsed comments
    fn comments(&mut self, comments: Vec<Newline>) -> Result<(), Self::Error>;

    /// Invoked when a simple word is parsed.
    fn word_fragment(
        &mut self,
        kind: WordKind<Self::Command, Self::WordFragment>,
    ) -> Result<Self::WordFragment, Self::Error>;

    /// Invoked when a concatenated word is parsed.
    // fn quote_word(&mut self, kind: ConcatWordKind<Self::WordFragment>) -> Result<Self::Word, Self::Error>;

    /// Invoked when a word is parsed.
    ///
    /// # Arguments
    /// * kind: the type of word that was parsed
    fn word(&mut self, kind: ConcatWordKind<Self::WordFragment>)
        -> Result<Self::Word, Self::Error>;

    /// Invoked when a redirect is parsed.
    ///
    /// # Arguments
    /// * kind: the type of redirect that was parsed
    fn redirect(&mut self, kind: RedirectKind<Self::Word>) -> Result<Self::Redirect, Self::Error>;
}

/// A trait which defines an interface which the parser defined in the `parse` module
/// uses to delegate Abstract Syntax Tree creation, especially m4 macro parts.
pub trait M4Builder: BuilderBase {
    /// A type for m4 macro call.
    type M4Macro;
    /// A type for m4 macro argument.
    type M4Argument;

    /// @kui8shi
    /// Invoked when a parsed m4 macro call is to be expanded to commands.
    ///
    /// # Arguments
    /// * macro_call: a m4 macro call
    fn macro_into_compound_command(
        &mut self,
        macro_call: Self::M4Macro,
        redirects: Vec<Self::Redirect>,
    ) -> Result<Self::CompoundCommand, Self::Error>;

    /// @kui8shi
    /// Invoked when a parsed m4 macro call is to be expanded to words.
    ///
    /// # Arguments
    /// * macro_call: a m4 macro call
    fn macro_into_word(
        &mut self,
        macro_call: Self::M4Macro,
    ) -> Result<Self::WordFragment, Self::Error>;

    /// @kui8shi
    /// Invoked when a m4 macro call is parsed.
    ///
    /// # Arguments
    /// * arg: a m4 macro argument
    fn macro_call(
        &mut self,
        name: String,
        args: Vec<M4Argument<Self::Command, Self::Word>>,
        effects: Option<SideEffect>,
        original_name: Option<String>,
    ) -> Result<Self::M4Macro, Self::Error>;
}

/// A trait which defines an interface which the parser defined in the `parse` module
/// uses to delegate Abstract Syntax Tree creation, especially makefile parts.
pub trait MakeBuilder: BuilderBase {
    /// The type which represents a top-level automake statement
    type Statement;

    /// Constructs an automake rule.
    fn rule(
        &mut self,
        target: Vec<Self::Word>,
        dependency: Vec<Self::Word>,
        recipe: Vec<Self::Statement>,
    ) -> Result<Self::Statement, Self::Error>;

    /// Constructs an automake conditional.
    fn conditional(
        &mut self,
        guard_var: String,
        then: Vec<Self::Statement>,
        otherwise: Vec<Self::Statement>,
    ) -> Result<Self::Statement, Self::Error>;

    /// Constructs an automake assignment.
    fn assignment(
        &mut self,
        lhs: String,
        op: AmAssignOp,
        rhs: Vec<Self::Word>,
    ) -> Result<Self::Statement, Self::Error>;

    /// Interpret a shell command as a part of automake recipe.
    fn shell_as_recipe(&mut self, cmd: Self::Command) -> Result<Self::Statement, Self::Error>;

    /// Construct an automake include statement.
    fn include(&mut self, path: Self::Word) -> Result<Self::Statement, Self::Error>;

    /// Construct an automake styled variable.
    fn variable(&mut self, var: AmVar) -> Result<Self::WordFragment, Self::Error>;
}

macro_rules! impl_builder_body {
    ($T:ident) => {
        type Command = $T::Command;
        type CompoundCommand = $T::CompoundCommand;
        type Word = $T::Word;
        type WordFragment = $T::WordFragment;
        type Redirect = $T::Redirect;
        type Error = $T::Error;
    };
}

macro_rules! impl_shell_builder_body {
    ($T:ident) => {
        type CommandList = $T::CommandList;
        type ListableCommand = $T::ListableCommand;
        type PipeableCommand = $T::PipeableCommand;

        fn complete_command(
            &mut self,
            pre_cmd_comments: Vec<Newline>,
            list: Self::CommandList,
            separator: SeparatorKind,
            cmd_comment: Option<Newline>,
            range: (usize, usize),
        ) -> Result<Self::Command, Self::Error> {
            (**self).complete_command(pre_cmd_comments, list, separator, cmd_comment, range)
        }

        fn and_or_list(
            &mut self,
            first: Self::ListableCommand,
            rest: Vec<(Vec<Newline>, AndOr<Self::ListableCommand>)>,
        ) -> Result<Self::CommandList, Self::Error> {
            (**self).and_or_list(first, rest)
        }

        fn pipeline(
            &mut self,
            bang: bool,
            cmds: Vec<(Vec<Newline>, Self::PipeableCommand)>,
        ) -> Result<Self::ListableCommand, Self::Error> {
            (**self).pipeline(bang, cmds)
        }

        fn simple_command(
            &mut self,
            redirects_or_env_vars: Vec<RedirectOrEnvVar<Self::Redirect, String, Self::Word>>,
            redirects_or_cmd_words: Vec<RedirectOrCmdWord<Self::Redirect, Self::Word>>,
        ) -> Result<Self::PipeableCommand, Self::Error> {
            (**self).simple_command(redirects_or_env_vars, redirects_or_cmd_words)
        }

        fn brace_group(
            &mut self,
            cmds: CommandGroup<Self::Command>,
            redirects: Vec<Self::Redirect>,
        ) -> Result<Self::CompoundCommand, Self::Error> {
            (**self).brace_group(cmds, redirects)
        }

        fn subshell(
            &mut self,
            cmds: CommandGroup<Self::Command>,
            redirects: Vec<Self::Redirect>,
        ) -> Result<Self::CompoundCommand, Self::Error> {
            (**self).subshell(cmds, redirects)
        }

        fn loop_command(
            &mut self,
            kind: LoopKind,
            guard_body_pair: GuardBodyPairGroup<Self::Command>,
            redirects: Vec<Self::Redirect>,
        ) -> Result<Self::CompoundCommand, Self::Error> {
            (**self).loop_command(kind, guard_body_pair, redirects)
        }

        fn if_command(
            &mut self,
            fragments: IfFragments<Self::Command>,
            redirects: Vec<Self::Redirect>,
        ) -> Result<Self::CompoundCommand, Self::Error> {
            (**self).if_command(fragments, redirects)
        }

        fn for_command(
            &mut self,
            fragments: ForFragments<Self::Command, Self::Word>,
            redirects: Vec<Self::Redirect>,
        ) -> Result<Self::CompoundCommand, Self::Error> {
            (**self).for_command(fragments, redirects)
        }

        fn case_command(
            &mut self,
            fragments: CaseFragments<Self::Command, Self::Word>,
            redirects: Vec<Self::Redirect>,
        ) -> Result<Self::CompoundCommand, Self::Error> {
            (**self).case_command(fragments, redirects)
        }

        fn compound_command_into_pipeable(
            &mut self,
            cmd: Self::CompoundCommand,
        ) -> Result<Self::PipeableCommand, Self::Error> {
            (**self).compound_command_into_pipeable(cmd)
        }

        fn function_declaration(
            &mut self,
            name: String,
            post_name_comments: Vec<Newline>,
            body: Self::CompoundCommand,
        ) -> Result<Self::PipeableCommand, Self::Error> {
            (**self).function_declaration(name, post_name_comments, body)
        }

        fn comments(&mut self, comments: Vec<Newline>) -> Result<(), Self::Error> {
            (**self).comments(comments)
        }

        fn word_fragment(
            &mut self,
            kind: WordKind<Self::Command, Self::WordFragment>,
        ) -> Result<Self::WordFragment, Self::Error> {
            (**self).word_fragment(kind)
        }

        fn word(
            &mut self,
            kind: ConcatWordKind<Self::WordFragment>,
        ) -> Result<Self::Word, Self::Error> {
            (**self).word(kind)
        }

        fn redirect(
            &mut self,
            kind: RedirectKind<Self::Word>,
        ) -> Result<Self::Redirect, Self::Error> {
            (**self).redirect(kind)
        }
    };
}

macro_rules! impl_m4_builder_body {
    ($T:ident) => {
        type M4Macro = $T::M4Macro;
        type M4Argument = $T::M4Argument;

        fn macro_into_compound_command(
            &mut self,
            macro_call: Self::M4Macro,
            redirects: Vec<Self::Redirect>,
        ) -> Result<Self::CompoundCommand, Self::Error> {
            (**self).macro_into_compound_command(macro_call, redirects)
        }

        fn macro_into_word(
            &mut self,
            macro_call: Self::M4Macro,
        ) -> Result<Self::WordFragment, Self::Error> {
            (**self).macro_into_word(macro_call)
        }

        fn macro_call(
            &mut self,
            name: String,
            args: Vec<M4Argument<Self::Command, Self::Word>>,
            effects: Option<SideEffect>,
            original_name: Option<String>,
        ) -> Result<Self::M4Macro, Self::Error> {
            (**self).macro_call(name, args, effects, original_name)
        }
    };
}

impl<T: BuilderBase + ?Sized> BuilderBase for &mut T {
    impl_builder_body!(T);
}
impl<T: BuilderBase + ?Sized> BuilderBase for Box<T> {
    impl_builder_body!(T);
}

impl<T: BuilderBase + M4Builder + M4Builder + ?Sized> M4Builder for &mut T {
    impl_m4_builder_body!(T);
}

impl<T: BuilderBase + ShellBuilder + M4Builder + ?Sized> M4Builder for Box<T> {
    impl_m4_builder_body!(T);
}

impl<T: BuilderBase + ShellBuilder + M4Builder + ?Sized> ShellBuilder for &mut T {
    impl_shell_builder_body!(T);
}

impl<T: BuilderBase + ShellBuilder + M4Builder + ?Sized> ShellBuilder for Box<T> {
    impl_shell_builder_body!(T);
}

/// error produced by ast builder
#[derive(Debug, Copy, Clone)]
pub enum BuilderError {
    /// when the selected builder has limited support
    UnsupportedSyntax,
}

impl Display for BuilderError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnsupportedSyntax => {
                write!(f, "builder does not support the syntax")
            }
        }
    }
}

#[must_use = "iterator adaptors are lazy and do nothing unless consumed"]
struct Coalesce<I: Iterator, F> {
    iter: I,
    cur: Option<I::Item>,
    func: F,
}

impl<I: Iterator, F> Coalesce<I, F> {
    fn new<T>(iter: T, func: F) -> Self
    where
        T: IntoIterator<IntoIter = I, Item = I::Item>,
    {
        Coalesce {
            iter: iter.into_iter(),
            cur: None,
            func,
        }
    }
}

type CoalesceResult<T> = Result<T, (T, T)>;
impl<I, F> Iterator for Coalesce<I, F>
where
    I: Iterator,
    F: FnMut(I::Item, I::Item) -> CoalesceResult<I::Item>,
{
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        let cur = self.cur.take().or_else(|| self.iter.next());
        let (mut left, mut right) = match (cur, self.iter.next()) {
            (Some(l), Some(r)) => (l, r),
            (Some(l), None) | (None, Some(l)) => return Some(l),
            (None, None) => return None,
        };

        loop {
            match (self.func)(left, right) {
                Ok(combined) => match self.iter.next() {
                    Some(next) => {
                        left = combined;
                        right = next;
                    }
                    None => return Some(combined),
                },

                Err((left, right)) => {
                    debug_assert!(self.cur.is_none());
                    self.cur = Some(right);
                    return Some(left);
                }
            }
        }
    }
}
