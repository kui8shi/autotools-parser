//! Defines node representations of the shell source.
use std::fmt::Debug;
use crate::ast::{
    minimal::{Condition, GuardBodyPair},
    PatternBodyPair, Redirect,
};
use crate::m4_macro::M4Macro;

/// Represents a unique node id
pub type NodeId = usize;
/// Wraps minimal Word with node id
pub type Word<L> = super::minimal::Word<L, NodeId>;

/// Complete the parsed command with additional information such as comment, line numbers, etc.
#[derive(Debug, Clone)]
pub struct Node<L> {
    /// trailing comments
    pub comment: Option<String>,
    /// range of line numbers in the original script.
    pub range: Option<(usize, usize)>,
    /// the command parsed
    pub kind: NodeKind<L>,
    /// the ids of children nodes
    pub children: Option<Vec<NodeId>>,
}

impl<L> Node<L> {
    /// Creates a new node instance
    pub fn new(
        comment: Option<String>,
        range: Option<(usize, usize)>,
        kind: NodeKind<L>,
        children: Option<Vec<NodeId>>,
    ) -> Self {
        Self {
            comment,
            range,
            kind,
            children,
        }
    }
}

/// represents any kinds of commands
#[derive(Debug, Clone)]
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
    While(GuardBodyPair<Word<L>, NodeId>),
    /// A until loop, represented as a guard-body pair.
    Until(GuardBodyPair<Word<L>, NodeId>),
    /// An if statement with one or more conditionals and an optional else branch.
    If {
        /// List of guard-body pairs for the if/else-if branches.
        conditionals: Vec<GuardBodyPair<Word<L>, NodeId>>,
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
        arms: Vec<PatternBodyPair<Word<L>, NodeId>>,
    },
    /// Executes a command if a condition holds (logical AND).
    And(Condition<Word<L>, NodeId>, NodeId),
    /// Executes a command if a condition holds (logical OR).
    Or(Condition<Word<L>, NodeId>, NodeId),
    /// Executes commands connecting stdout/in via a pipe.
    Pipe(bool, Vec<NodeId>),
    /// A command with associated redirections.
    Redirect(NodeId, Vec<Redirect<Word<L>>>),
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
    Macro(M4Macro<Word<L>, NodeId>),
}
