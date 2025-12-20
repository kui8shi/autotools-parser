//! Defines abstract representations of the automake source.
use std::fmt::Display;

use super::minimal::{Word, WordFragment};
use super::node::{NodeId, ShellCommand};

/// Wraps word fragment with fixing generics
pub type AmWordFragment = MayAm<WordFragment<String, NodeId, AmWord>, AmVar>;

/// Wraps minimal word with fixing generics
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct AmWord(pub Word<AmWordFragment>);

impl From<Word<AmWordFragment>> for AmWord {
    fn from(value: Word<AmWordFragment>) -> Self {
        Self(value)
    }
}

impl Into<Word<AmWordFragment>> for AmWord {
    fn into(self) -> Word<AmWordFragment> {
        self.0
    }
}

impl Into<Option<WordFragment<String, NodeId, AmWord>>> for AmWord {
    fn into(self) -> Option<WordFragment<String, NodeId, AmWord>> {
        use MayAm::*;
        use Word::*;
        match self.0 {
            Single(Shell(word)) => Some(word),
            _ => None,
        }
    }
}

impl Into<Option<WordFragment<String, NodeId, AmWord>>> for AmWordFragment {
    fn into(self) -> Option<WordFragment<String, NodeId, AmWord>> {
        use MayAm::*;
        match self {
            Shell(word) => Some(word),
            _ => None,
        }
    }
}

impl Into<Option<String>> for AmWordFragment {
    fn into(self) -> Option<String> {
        use MayAm::*;
        use WordFragment::*;
        match self {
            Shell(Literal(l)) => Some(l.into()),
            _ => None,
        }
    }
}

impl Into<Option<String>> for AmWord {
    fn into(self) -> Option<String> {
        match self.0 {
            Word::Empty => Some(String::new()),
            Word::Concat(_) => None,
            Word::Single(word) => word.into(),
        }
    }
}

impl From<String> for AmWord {
    fn from(value: String) -> Self {
        value
            .is_empty()
            .then_some(Word::Empty)
            .unwrap_or(Word::Single(MayAm::Shell(WordFragment::Literal(
                value.into(),
            ))))
            .into()
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
/// Represents different types of automake statements.
pub enum AmLine {
    /// Represents a make rule statement in automake.
    Rule(AmRule),
    /// Represents a conditional statement in automake.
    Conditional(AmConditional),
    /// Represents an assignment statement in automake.
    Assignment(AmAssignment),
    /// Represents an include statement in automake.
    Include(AmWord),
    /// Represents a shell command in a rule. Note that this can NOT be the top-level statement
    Shell(ShellCommand<AmWord>),
}

impl From<ShellCommand<AmWord>> for AmLine {
    fn from(value: ShellCommand<AmWord>) -> Self {
        Self::Shell(value)
    }
}

impl Into<Option<ShellCommand<AmWord>>> for AmLine {
    fn into(self) -> Option<ShellCommand<AmWord>> {
        use AmLine::*;
        match self {
            Shell(cmd) => Some(cmd),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
/// Represents a make rule in automake with target, dependencies, and recipe.
pub struct AmRule {
    /// The target of the rule (e.g., 'all', 'install', 'clean').
    pub target: Vec<AmWord>,
    /// The dependencies required by this rule.
    pub dependency: Vec<AmWord>,
    /// The recipe commands to execute for this rule.
    pub recipe: Vec<NodeId>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
/// Represents a conditional statement in automake (if/else).
pub struct AmConditional {
    /// The variable to test in the conditional.
    pub guard_var: String,
    /// The statements to execute if the condition is true.
    pub then: Vec<NodeId>,
    /// The statements to execute if the condition is false.
    pub otherwise: Vec<NodeId>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
/// Represents a variable assignment in automake.
pub struct AmAssignment {
    /// The left-hand side (variable name) of the assignment.
    pub lhs: AmWord,
    /// The assignment operator (=, :=, +=).
    pub op: AmAssignOp,
    /// The right-hand side (value) of the assignment.
    pub rhs: Vec<AmWord>,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
/// Represents different assignment operators in automake.
pub enum AmAssignOp {
    /// =
    Lazy,
    /// :=
    Instant,
    /// +=
    Append,
}

#[derive(Debug, PartialEq, Eq, Clone)]
/// Represents a value that can be either a shell command or automake construct.
pub enum MayAm<S, A> {
    /// A shell command.
    Shell(S),
    /// An automake construct.
    Automake(A),
}

impl From<WordFragment<String, NodeId, AmWord>> for AmWordFragment {
    fn from(value: WordFragment<String, NodeId, AmWord>) -> Self {
        Self::Shell(value)
    }
}

/// Some of the special makefile parameters could have suffix of "D" or "F".
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum MakeDF {
    /// e.g. $(@D)
    Dir,
    /// e.g. $(^F)
    File,
}

impl Display for MakeDF {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use MakeDF::*;
        match self {
            Dir => write!(f, "D"),
            File => write!(f, "F"),
        }
    }
}

/// Used to indicate what kind of makefile parameter could be parsed next.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum MakeParameter {
    /// $@ or $(@)
    Target(Option<MakeDF>),
    /// $* or $(*)
    Match(Option<MakeDF>),
    /// $< or $(<)
    FirstDependency(Option<MakeDF>),
    /// $^ or $(^)
    AllDependency(Option<MakeDF>),
    /// $+ or $(+)
    AllDependencyAllowingDuplicate(Option<MakeDF>),
    /// $? or $(?)
    NewerDependency(Option<MakeDF>),
    /// $(foo)
    Var(String),
}

impl Display for MakeParameter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use MakeParameter::*;
        let (inner, should_enclose, df) = match self {
            Target(df) => ("@", df.is_some(), df),
            Match(df) => ("*", df.is_some(), df),
            FirstDependency(df) => ("<", df.is_some(), df),
            AllDependency(df) => ("^", df.is_some(), df),
            AllDependencyAllowingDuplicate(df) => ("+", df.is_some(), df),
            NewerDependency(df) => ("?", df.is_some(), df),
            Var(s) => (s.as_str(), true, &None),
        };
        if should_enclose {
            if let Some(df) = df {
                // df is some
                write!(f, "$({}{})", inner, df)
            } else {
                // var
                write!(f, "$({})", inner)
            }
        } else {
            // df is none
            write!(f, "${}", inner)
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
/// Represents different types of automake variables.
pub enum AmVar {
    /// A regular variable reference.
    Param(MakeParameter),
    /// A template variable reference.
    Template(String),
}

impl Display for AmVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use AmVar::*;
        match self {
            Param(s) => write!(f, "{}", s),
            Template(s) => write!(f, "@{}@", s),
        }
    }
}
