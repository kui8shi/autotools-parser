//! Defines minimal representations of the shell source.
use super::{Arithmetic, Parameter, ParameterSubstitution, PatternBodyPair, Redirect};
use crate::m4_macro::M4Macro;

/// Represents the smallest fragment of any text.
///
/// Generic over the representation of a literals, parameters, and substitutions.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum WordFragment<L, W, C> {
    /// A non-special literal word.
    Literal(L),
    /// A list of simple words concatenated by double quotes.
    /// While we treat double quoted literals as they are, we unwrap single quoted literals.
    DoubleQuoted(Vec<Self>),
    /// A token which normally has a special meaning is treated as a literal
    /// because it was escaped, typically with a backslash, e.g. `\"`.
    Escaped(L),
    /// Access of a value inside a parameter, e.g. `$foo` or `$$`.
    Param(Parameter<L>),
    /// A parameter substitution, e.g. `${param-word}`.
    Subst(Box<ParameterSubstitution<Parameter<L>, W, C, Arithmetic<L>>>),
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
    /// m4 macro call to be expanded to a literal
    Macro(M4Macro<W, C>),
}

/// A collection of simple words, wrapping a vector of `WordFragment`
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Word<T, C> {
    /// A word composed of multiple fragments concatenated together
    Concat(Vec<WordFragment<T, Self, C>>),
    /// A word containing exactly one fragment
    Single(WordFragment<T, Self, C>),
    /// A word which is equivalent to an empty strng, such as '', "".
    Empty,
}

impl<T, C> Word<T, C>
where
    T: PartialEq<str>,
{
    /// return true if the word is literal and equal to the given literal.
    pub fn is_literal(&self, literal: &str) -> bool {
        match self {
            Word::Single(WordFragment::Literal(s)) if s == literal => true,
            _ => false,
        }
    }
}

/// Operators used to compare words or check file properties.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Operator<W> {
    /// Equality operator (==) between two words.
    Eq(W, W),
    /// Inequality operator (!=) between two words.
    Neq(W, W),
    /// Greater than or equal operator (>=) between two words.
    Ge(W, W),
    /// Greater than operator (>) between two words.
    Gt(W, W),
    /// Less than or equal operator (<=) between two words.
    Le(W, W),
    /// Less than operator (<) between two words.
    Lt(W, W),
    /// Checks if the given word is empty.
    Empty(W),
    /// Checks if the given word is non-empty.
    NonEmpty(W),
    /// Checks if the given word represents an existing directory.
    Dir(W),
    /// Checks if the given word represents an existing file.
    File(W),
    /// Checks if the given word represents a non-existing path.
    NoExists(W),
}

/// Represents a condition for control flow, which can be a single operator-based
/// condition, a logical combination of conditions, or an evaluated word.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Condition<W, C> {
    /// A single condition based on an operator.
    Cond(Operator<W>),
    /// Logical AND of two conditions.
    And(Box<Self>, Box<Self>),
    /// Logical OR of two conditions.
    Or(Box<Self>, Box<Self>),
    /// Evaluates the commands first, then treat the result output as a condition.
    Eval(Vec<C>),
    /// Evaluates the commands, then take the return code as a boolean condition.
    ReturnZero(Box<C>),
}

/// A pairing of a condition (guard) with a block of commands (body).
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct GuardBodyPair<W, C> {
    /// The condition to evaluate.
    pub condition: Condition<W, C>,
    /// The commands to execute if the condition evaluates to true.
    pub body: Vec<C>,
}

/// Represents a compound command which includes control flow constructs such as loops,
/// conditionals, case statements, and background execution.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum CompoundCommand<W, C> {
    /// A group of commands that should be executed in the current environment.
    Brace(Vec<C>),
    /// A group of commands that should be executed in a subshell environment.
    Subshell(Vec<C>),
    /// A while loop, represented as a guard-body pair.
    While(GuardBodyPair<W, C>),
    /// A until loop, represented as a guard-body pair.
    Until(GuardBodyPair<W, C>),
    /// An if statement with one or more conditionals and an optional else branch.
    If {
        /// List of guard-body pairs for the if/else-if branches.
        conditionals: Vec<GuardBodyPair<W, C>>,
        /// Commands to execute if none of the conditions are met (else branch).
        else_branch: Vec<C>,
    },
    /// A for loop that iterates over a list of words.
    For {
        /// The loop variable name.
        var: String,
        /// The list of words to iterate over.
        words: Vec<W>,
        /// The commands to execute in each iteration.
        body: Vec<C>,
    },
    /// A case statement for pattern matching.
    Case {
        /// The word to match against the provided patterns.
        word: W,
        /// A list of pattern-body pairs.
        arms: Vec<PatternBodyPair<W, C>>,
    },
    /// Executes a command if a condition holds (logical AND).
    And(Condition<W, C>, Box<C>),
    /// Executes a command if a condition holds (logical OR).
    Or(Condition<W, C>, Box<C>),
    /// Executes commands connecting stdout/in via a pipe.
    Pipe(bool, Vec<C>),
    /// A command with associated redirections.
    Redirect(Box<C>, Vec<Redirect<W>>),
    /// A command that is executed in the background.
    Background(Box<C>),
    /// A function declaration
    FunctionDef {
        /// The function name
        name: String,
        /// Commands in the body
        body: Box<C>,
    },
    /// A macro call utilizing M4 macros.
    Macro(M4Macro<W, C>),
}

/// Complete the parsed command with additional information such as comment, line numbers, etc.
#[derive(Debug, Clone)]
pub struct CommandWrapper<V> {
    /// trailing comments
    pub comment: Option<String>,
    /// range of line numbers in the original script.
    pub range: Option<(usize, usize)>,
    /// the command parsed
    pub cmd: Command<V>,
}

impl<V: Eq> Eq for CommandWrapper<V> {}

impl<V: PartialEq> PartialEq for CommandWrapper<V> {
    fn eq(&self, other: &Self) -> bool {
        self.cmd == other.cmd
    }
}

impl<V> CommandWrapper<V> {
    /// Create a new command wrapper without any extra information
    pub fn new(cmd: Command<V>) -> Self {
        Self::new_with_comment(cmd, None)
    }

    /// Wrap a command with its trailing comments
    pub fn new_with_comment(cmd: Command<V>, comment: Option<String>) -> Self {
        Self {
            comment,
            range: None,
            cmd,
        }
    }
}

/// Represents a command, which can be an assignment, compound command, simple command,
/// or macro command.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Command<V> {
    /// An assignment command that associates a value with a variable.
    Assignment(V, Word<V, CommandWrapper<V>>),
    /// A compound command such as loops, conditionals, or case statements.
    Compound(CompoundCommand<Word<V, CommandWrapper<V>>, CommandWrapper<V>>),
    /// A simple command represented by a sequence of words.
    Cmd(Vec<Word<V, CommandWrapper<V>>>),
}
