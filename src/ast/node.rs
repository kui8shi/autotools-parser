//! Defines node representations of the shell source.
use crate::m4_macro;
use std::fmt::Debug;

/// Represents a unique node id
pub type NodeId = usize;
/// Wraps minimal Word with fixing generics
pub type Word<L> = super::minimal::Word<L, NodeId>;
/// Wraps minimal word fragment with fixing generics
pub type WordFragment<L> = super::minimal::WordFragment<L, Word<L>, NodeId>;
/// Wraps minimal condition with fixing generics
pub type Condition<L> = super::minimal::Condition<Word<L>, NodeId>;
/// Wraps minimal operator with fixing generics
pub type Operator<L> = super::minimal::Operator<Word<L>>;
/// Wraps minimal guard body pair with fixing generics
pub type GuardBodyPair<L> = super::minimal::GuardBodyPair<Word<L>, NodeId>;
/// Wraps pattern body pair with fixing generics
pub type PatternBodyPair<L> = super::PatternBodyPair<Word<L>, NodeId>;
/// Wraps redirect with fixing generics
pub type Redirect<L> = super::Redirect<Word<L>>;
/// Wraps parameter substitution with fixing generics
pub type ParameterSubstitution<L> =
    super::ParameterSubstitution<super::Parameter<L>, Word<L>, NodeId, super::Arithmetic<L>>;
/// Wraps m4 macro with fixing generics
pub type M4Macro<L> = m4_macro::M4Macro<Word<L>, NodeId>;
/// Wraps m4 argument with fixing generics
pub type M4Argument<L> = m4_macro::M4Argument<Word<L>, NodeId>;

/// Complete the parsed command with additional information such as comment, line numbers, etc.
#[derive(Debug, Clone)]
pub struct Node<L> {
    /// trailing comments
    pub comment: Option<String>,
    /// range of line numbers in the original script.
    pub range: Option<(usize, usize)>,
    /// the command parsed
    pub kind: NodeKind<L>,
}

impl<L> Node<L> {
    /// Creates a new node instance
    pub fn new(comment: Option<String>, range: Option<(usize, usize)>, kind: NodeKind<L>) -> Self {
        Self {
            comment,
            range,
            kind,
        }
    }
}

/// represents any kinds of commands
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NodeKind<L> {
    /// An assignment command that associates a value with a variable.
    Assignment(L, Word<L>),
    /// A simple command represented by a sequence of words.
    Cmd(Vec<Word<L>>),
    /// A group of commands that should be executed in the current environment.
    Brace(Vec<NodeId>),
    /// A group of commands that should be executed in a subshell environment.
    Subshell(Vec<NodeId>),
    /// A while loop, represented as a guard-body pair.
    While(GuardBodyPair<L>),
    /// A until loop, represented as a guard-body pair.
    Until(GuardBodyPair<L>),
    /// An if statement with one or more conditionals and an optional else branch.
    If {
        /// List of guard-body pairs for the if/else-if branches.
        conditionals: Vec<GuardBodyPair<L>>,
        /// Commands to execute if none of the conditions are met (else branch).
        else_branch: Vec<NodeId>,
    },
    /// A for loop that iterates over a list of words.
    For {
        /// The loop variable name.
        var: String,
        /// The list of words to iterate over.
        words: Vec<Word<L>>,
        /// The commands to execute in each iteration.
        body: Vec<NodeId>,
    },
    /// A case statement for pattern matching.
    Case {
        /// The word to match against the provided patterns.
        word: Word<L>,
        /// A list of pattern-body pairs.
        arms: Vec<PatternBodyPair<L>>,
    },
    /// Executes a command if a condition holds (logical AND).
    And(Condition<L>, NodeId),
    /// Executes a command if a condition holds (logical OR).
    Or(Condition<L>, NodeId),
    /// Executes commands connecting stdout/in via a pipe.
    Pipe(bool, Vec<NodeId>),
    /// A command with associated redirections.
    Redirect(NodeId, Vec<Redirect<L>>),
    /// A command that is executed in the background.
    Background(NodeId),
    /// A function declaration
    FunctionDef {
        /// The function name
        name: String,
        /// Commands in the body
        body: NodeId,
    },
    /// A macro call utilizing M4 macros.
    Macro(M4Macro<L>),
}
