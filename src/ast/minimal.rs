//! Defines minimal representations of the shell source.
use super::{Arithmetic, MayM4, Parameter, ParameterSubstitution, PatternBodyPair, Redirect};
use crate::m4_macro::M4Macro;
use std::fmt;

/// Represents the smallest fragment of any text.
///
/// Generic over the representation of a literals, parameters, and substitutions.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum WordFragment<L, C, W> {
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
    Subst(Box<ParameterSubstitution<Parameter<L>, C, W, Arithmetic<L>>>),
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

impl<L, C, W> Into<Option<String>> for MayM4<WordFragment<L, C, W>, M4Macro<C, W>>
where
    L: Into<String>,
{
    fn into(self) -> Option<String> {
        use MayM4::*;
        use WordFragment::*;
        match self {
            Shell(Literal(l)) => Some(l.into()),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
/// Wraps minimal Word with fixing generics
pub struct AcWord<L>(pub Word<MinimalWordFragment<L>>);

/// Wraps command with fixing generics
pub type MinimalCommand<L> = AcCommand<L, AcWord<L>>;

/// Wraps compound command with fixing generics
pub type MinimalCompoundCommand<L> =
    MayM4<CompoundCommand<MinimalCommand<L>, AcWord<L>>, M4Macro<MinimalCommand<L>, AcWord<L>>>;

/// Wraps word fragment with fixing generics
pub type MinimalWordFragment<L> = MayM4<
    WordFragment<L, AcCommand<L, AcWord<L>>, AcWord<L>>,
    M4Macro<MinimalCommand<L>, AcWord<L>>,
>;

impl<L> From<Word<MinimalWordFragment<L>>> for AcWord<L> {
    fn from(value: Word<MinimalWordFragment<L>>) -> Self {
        Self(value)
    }
}

impl<L> From<AcWord<L>> for Word<MinimalWordFragment<L>> {
    fn from(value: AcWord<L>) -> Self {
        value.0
    }
}

/// A collection of simple words, wrapping a vector of `WordFragment`
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Word<X> {
    /// A word composed of multiple fragments concatenated together
    Concat(Vec<X>),
    /// A word containing exactly one fragment
    Single(X),
    /// A word which is equivalent to an empty strng, such as '', "".
    Empty,
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
pub enum Condition<C, W> {
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
pub struct GuardBodyPair<C, W> {
    /// The condition to evaluate.
    pub condition: Condition<C, W>,
    /// The commands to execute if the condition evaluates to true.
    pub body: Vec<C>,
}

/// Represents a compound command which includes control flow constructs such as loops,
/// conditionals, case statements, and background execution.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum CompoundCommand<C, W> {
    /// A group of commands that should be executed in the current environment.
    Brace(Vec<C>),
    /// A group of commands that should be executed in a subshell environment.
    Subshell(Vec<C>),
    /// A while loop, represented as a guard-body pair.
    While(GuardBodyPair<C, W>),
    /// A until loop, represented as a guard-body pair.
    Until(GuardBodyPair<C, W>),
    /// An if statement with one or more conditionals and an optional else branch.
    If {
        /// List of guard-body pairs for the if/else-if branches.
        conditionals: Vec<GuardBodyPair<C, W>>,
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
        arms: Vec<PatternBodyPair<C, W>>,
    },
    /// Executes a command if a condition holds (logical AND).
    And(Condition<C, W>, Box<C>),
    /// Executes a command if a condition holds (logical OR).
    Or(Condition<C, W>, Box<C>),
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
}

/// Complete the parsed command with additional information such as comment, line numbers, etc.
#[derive(Debug, Clone)]
pub struct AcCommand<L, W> {
    /// trailing comments
    pub comment: Option<String>,
    /// range of line numbers in the original script.
    pub range: Option<(usize, usize)>,
    /// the command parsed
    pub cmd: Box<Command<L, MayM4<CompoundCommand<Self, W>, M4Macro<Self, W>>, W>>,
}

impl<L: Eq, W: Eq> Eq for AcCommand<L, W> {}

impl<L: PartialEq, W: PartialEq> PartialEq for AcCommand<L, W> {
    fn eq(&self, other: &Self) -> bool {
        self.cmd == other.cmd
    }
}

impl<L, W> AcCommand<L, W> {
    /// Creates a new assignment command with the given name and word.
    pub fn new_assign(name: L, word: W) -> Self {
        Self::new(Command::Assignment(name, word), None)
    }

    /// Creates a new may m4 compound command from either a compound command or M4 macro.
    pub fn new_may_m4(cmd: MayM4<CompoundCommand<Self, W>, M4Macro<Self, W>>) -> Self {
        Self::new(Command::Compound(cmd), None)
    }

    /// Creates a new compound command
    pub fn new_compound(cmd: CompoundCommand<Self, W>) -> Self {
        Self::new(Command::Compound(MayM4::Shell(cmd)), None)
    }

    /// Creates a new command from an M4 macro.
    pub fn new_macro(m4_macro: M4Macro<Self, W>) -> Self {
        Self::new(Command::Compound(MayM4::Macro(m4_macro)), None)
    }

    /// Creates a new simple command from a vector of words.
    pub fn new_cmd(cmd_words: Vec<W>) -> Self {
        Self::new(Command::Cmd(cmd_words), None)
    }

    /// Wrap a command with its trailing comments
    pub fn new(
        cmd: Command<L, MayM4<CompoundCommand<Self, W>, M4Macro<Self, W>>, W>,
        comment: Option<String>,
    ) -> Self {
        Self {
            comment,
            range: None,
            cmd: Box::new(cmd),
        }
    }
}

/// Represents a command, which can be an assignment, compound command, simple command,
/// or macro command.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Command<L, C, W> {
    /// An assignment command that associates a value with a variable.
    Assignment(L, W),
    /// A compound command such as loops, conditionals, or case statements.
    Compound(C),
    /// A simple command represented by a sequence of words.
    Cmd(Vec<W>),
}

impl<W> fmt::Display for Operator<W>
where
    W: fmt::Display,
{
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        use self::Operator::*;
        match self {
            Eq(lhs, rhs) => write!(fmt, "{} = {}", lhs, rhs),
            Neq(lhs, rhs) => write!(fmt, "{} != {}", lhs, rhs),
            Ge(lhs, rhs) => write!(fmt, "{} -ge {}", lhs, rhs),
            Gt(lhs, rhs) => write!(fmt, "{} -gt {}", lhs, rhs),
            Le(lhs, rhs) => write!(fmt, "{} -le {}", lhs, rhs),
            Lt(lhs, rhs) => write!(fmt, "{} -lt {}", lhs, rhs),
            Empty(w) => write!(fmt, "-z {}", w),
            NonEmpty(w) => write!(fmt, "-n {}", w),
            Dir(w) => write!(fmt, "-d {}", w),
            File(w) => write!(fmt, "-f {}", w),
            NoExists(w) => write!(fmt, "! -e {}", w),
        }
    }
}

impl<C, W> fmt::Display for Condition<C, W>
where
    W: fmt::Display,
    C: fmt::Display,
{
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        use self::Condition::*;
        match self {
            Cond(op) => write!(fmt, "test {}", op),
            And(lhs, rhs) => write!(fmt, "{} && {}", lhs, rhs),
            Or(lhs, rhs) => write!(fmt, "{} || {}", lhs, rhs),
            Eval(cmds) => write!(
                fmt,
                "eval \"{}\"",
                cmds.iter()
                    .map(|c| c.to_string())
                    .collect::<Vec<String>>()
                    .join(";")
            ),
            ReturnZero(cmd) => write!(fmt, "{}", cmd),
        }
    }
}
