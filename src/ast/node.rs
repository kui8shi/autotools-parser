//! Defines node representations of the shell source.
use std::cell::Cell;

use crate::m4_macro;
use slab::Slab;

use super::minimal::Word;

/// Represents a unique node id
pub type NodeId = usize;
/// Wraps minimal word fragment with fixing generics
pub type WordFragment<W> = super::minimal::WordFragment<String, NodeId, W>;
/// Wraps minimal condition with fixing generics
pub type Condition<W> = super::condition::Condition<NodeId, W>;
/// Wraps minimal operator with fixing generics
pub type Operator<W> = super::condition::Operator<W>;
/// Wraps minimal guard body pair with fixing generics
pub type GuardBodyPair<W> = super::minimal::GuardBodyPair<NodeId, W>;
/// Wraps pattern body pair with fixing generics
pub type PatternBodyPair<W> = super::PatternBodyPair<NodeId, W>;
/// Wraps redirect with fixing generics
pub type Redirect<W> = super::Redirect<W>;
/// Wraps parameter substitution with fixing generics
pub type ParameterSubstitution<W> =
    super::ParameterSubstitution<super::Parameter<String>, NodeId, W, super::Arithmetic<String>>;
/// Wraps m4 macro with fixing generics
pub type M4Macro = m4_macro::M4Macro<NodeId, AcWord>;
/// Wraps m4 argument with fixing generics
pub type M4Argument = m4_macro::M4Argument<NodeId, AcWord>;
/// Wraps minimal word fragment with fixing generics
pub type AcWordFragment = super::MayM4<WordFragment<AcWord>, M4Macro>;

/// Wraps minimal Word with fixing generics
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct AcWord(pub Word<AcWordFragment>);

impl From<WordFragment<AcWord>> for AcWordFragment {
    fn from(value: WordFragment<AcWord>) -> Self {
        super::MayM4::Shell(value)
    }
}

impl From<Word<AcWordFragment>> for AcWord {
    fn from(value: Word<AcWordFragment>) -> Self {
        Self(value)
    }
}

impl From<&Word<AcWordFragment>> for AcWord {
    fn from(value: &Word<AcWordFragment>) -> Self {
        Self(value.clone())
    }
}

impl From<AcWord> for Word<AcWordFragment> {
    fn from(value: AcWord) -> Self {
        value.0
    }
}

impl Into<Option<WordFragment<AcWord>>> for AcWordFragment {
    fn into(self) -> Option<WordFragment<AcWord>> {
        use super::MayM4::*;
        match self {
            Shell(word) => Some(word),
            Macro(_) => None,
        }
    }
}

impl Into<Option<String>> for AcWord {
    fn into(self) -> Option<String> {
        match self.0 {
            Word::Empty => Some(String::new()),
            Word::Concat(_) => None,
            Word::Single(word) => word.into(),
        }
    }
}

impl From<String> for AcWord {
    fn from(value: String) -> Self {
        value
            .is_empty()
            .then_some(Word::Empty)
            .unwrap_or(Word::Single(super::MayM4::Shell(WordFragment::Literal(
                value.into(),
            ))))
            .into()
    }
}

/// Wraps command with fixing generics
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct AcCommand(pub super::MayM4<ShellCommand<AcWord>, M4Macro>);

impl AcCommand {
    /// Creates a new command from a shell command.
    pub fn new_cmd(cmd: ShellCommand<AcWord>) -> Self {
        Self(super::MayM4::Shell(cmd))
    }
}

impl From<ShellCommand<AcWord>> for AcCommand {
    /// Creates a new autoconf command from a shell command.
    fn from(value: ShellCommand<AcWord>) -> Self {
        Self(super::MayM4::Shell(value))
    }
}

impl Into<Option<ShellCommand<AcWord>>> for AcCommand {
    fn into(self) -> Option<ShellCommand<AcWord>> {
        match self.0 {
            super::MayM4::Shell(cmd) => Some(cmd),
            super::MayM4::Macro(_) => None,
        }
    }
}

impl From<m4_macro::M4Macro<NodeId, AcWord>> for AcCommand {
    fn from(value: M4Macro) -> Self {
        Self(super::MayM4::Macro(value))
    }
}

/// Complete the parsed command with additional information such as comment, line numbers, etc.
#[derive(Debug, Clone)]
pub struct Node<C, U> {
    /// trailing comments
    pub comment: Option<String>,
    /// range of line numbers in the original script.
    pub range: Vec<(usize, usize)>,
    /// the command parsed
    pub cmd: C,
    /// extra information (put user-defined struct here)
    pub info: U,
}

impl<C, U> Node<C, U> {
    /// Creates a new node instance
    pub fn new(comment: Option<String>, range: Option<(usize, usize)>, cmd: C, info: U) -> Self {
        Self {
            comment,
            range: range.map_or(Vec::new(), |r| vec![r]),
            cmd,
            info,
        }
    }

    /// Get the minimum line number in `range`
    pub fn range_start(&self) -> Option<usize> {
        self.range.first().map(|(start, _)| start).copied()
    }

    /// Get the maximum line number in `range`
    pub fn range_end(&self) -> Option<usize> {
        self.range.last().map(|(_, end)| end).copied()
    }
}

/// represents any kinds of commands
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ShellCommand<W> {
    /// An assignment command that associates a value with a variable.
    Assignment(String, W),
    /// A simple command represented by a sequence of words.
    Cmd(Vec<W>),
    /// A group of commands that should be executed in the current environment.
    Brace(Vec<NodeId>),
    /// A group of commands that should be executed in a subshell environment.
    Subshell(Vec<NodeId>),
    /// A while loop, represented as a guard-body pair.
    While(GuardBodyPair<W>),
    /// A until loop, represented as a guard-body pair.
    Until(GuardBodyPair<W>),
    /// An if statement with one or more conditionals and an optional else branch.
    If {
        /// List of guard-body pairs for the if/else-if branches.
        conditionals: Vec<GuardBodyPair<W>>,
        /// Commands to execute if none of the conditions are met (else branch).
        else_branch: Vec<NodeId>,
    },
    /// A for loop that iterates over a list of words.
    For {
        /// The loop variable name.
        var: String,
        /// The list of words to iterate over.
        words: Vec<W>,
        /// The commands to execute in each iteration.
        body: Vec<NodeId>,
    },
    /// A case statement for pattern matching.
    Case {
        /// The word to match against the provided patterns.
        word: W,
        /// A list of pattern-body pairs.
        arms: Vec<PatternBodyPair<W>>,
    },
    /// Executes a command if a condition holds (logical AND).
    And(Condition<W>, NodeId),
    /// Executes a command if a condition holds (logical OR).
    Or(Condition<W>, NodeId),
    /// Executes commands connecting stdout/in via a pipe.
    Pipe(bool, Vec<NodeId>),
    /// A command with associated redirections.
    Redirect(NodeId, Vec<Redirect<W>>),
    /// A command that is executed in the background.
    Background(NodeId),
    /// A function declaration
    FunctionDef {
        /// The function name
        name: String,
        /// Commands in the body
        body: NodeId,
    },
}

/// Trait for displaying nodes in a formatted way.
pub trait DisplayNode {
    /// The word type used for display.
    type Word;

    /// Display a node with the given ID and indentation level.
    fn display_node(&self, node_id: NodeId, indent_level: usize) -> String;

    /// Display a word, optionally with quotes.
    fn display_word(&self, word: &Self::Word, should_quote: bool) -> String;
}

/// A pool of autoconf commands stored as nodes.
pub struct AutoconfPool<U = ()> {
    /// Contains all nodes. `NodeId` represents indexes of nodes in this slab.
    pub nodes: Slab<Node<AcCommand, U>>,
    /// Assigned the id of top-most node while formatting a tree of nodes.
    focus: Cell<Option<NodeId>>,
    /// A user-defined function which tell the node should be displayed
    beyond_boundary: Option<Box<dyn Fn(&Node<AcCommand, U>) -> bool>>,
}

impl<U: std::fmt::Debug> std::fmt::Debug for AutoconfPool<U> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("AutoconfPool")
            .field("nodes", &self.nodes)
            .field("forcus", &self.focus)
            .finish()
    }
}

impl<U> AutoconfPool<U> {
    /// Construct a new pool of autoconf commands
    pub fn new(
        nodes: Slab<Node<AcCommand, U>>,
        beyond_boundary: Option<Box<dyn Fn(&Node<AcCommand, U>) -> bool>>,
    ) -> Self {
        Self {
            nodes,
            focus: Cell::new(None),
            beyond_boundary,
        }
    }

    /// Construct a new pool from a given vector of autoconf commands
    pub fn from_vec(nodes: Vec<Node<AcCommand, U>>) -> Self {
        let nodes = {
            let mut tmp = Slab::with_capacity(nodes.len());
            for node in nodes {
                tmp.insert(node);
            }
            tmp
        };
        Self {
            nodes,
            focus: Cell::new(None),
            beyond_boundary: None,
        }
    }

    /// Get the number of nodes (commands)
    pub fn num_nodes(&self) -> usize {
        self.nodes.len()
    }

    /// Get a node by its ID.
    pub fn get(&self, id: NodeId) -> Option<&Node<AcCommand, U>> {
        self.nodes.get(id)
    }
}

impl<U> DisplayNode for AutoconfPool<U> {
    type Word = AcWord;

    fn display_node(&self, node_id: NodeId, indent_level: usize) -> String {
        if let Some(node) = self.get(node_id) {
            let is_top = self.focus.get().is_none();
            if is_top {
                self.focus.replace(Some(node_id));
            } else {
                if let Some(beyond_boundary) = &self.beyond_boundary {
                    if beyond_boundary(node) {
                        // the node beyond boundary should not be displayed.
                        return String::new();
                    }
                }
            }
            use super::MayM4::*;
            let result = match &node.cmd.0 {
                Macro(m4_macro) => self.m4_macro_to_string(m4_macro, indent_level),
                Shell(cmd) => self.command_to_string(cmd, node.comment.clone(), indent_level),
            };
            if is_top {
                self.focus.replace(None);
            }
            result
        } else {
            String::new()
        }
    }

    fn display_word(&self, word: &AcWord, should_quote: bool) -> String {
        use super::minimal::WordFragment::{DoubleQuoted, Literal};
        use Word::*;
        match &word.0 {
            Empty => "\"\"".to_string(),
            Concat(frags) => {
                format!(
                    "{}",
                    frags
                        .iter()
                        .map(|w| self.may_m4_word_to_string(w))
                        .collect::<Vec<String>>()
                        .concat()
                )
            }
            Single(frag) => {
                let s = self.may_m4_word_to_string(frag);
                if should_quote && !matches!(frag, super::MayM4::Shell(DoubleQuoted(_))) {
                    format!("\"{}\"", s)
                } else if should_quote && !matches!(frag, super::MayM4::Shell(Literal(_))) {
                    format!("\'{}\'", s)
                } else {
                    s
                }
            }
        }
    }
}

impl<U> AutoconfPool<U> {
    fn may_m4_word_to_string(&self, frag: &AcWordFragment) -> String {
        use super::MayM4::*;
        match frag {
            Macro(macro_word) => format!(
                "{}({})",
                macro_word.name,
                macro_word
                    .args
                    .iter()
                    .map(|arg| self.m4_argument_to_string(arg, 0))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Shell(shell_word) => self.shell_word_to_string(shell_word),
        }
    }

    /// Format an M4 macro call (`M4Macro`) into a string
    fn m4_macro_to_string(&self, m4_macro: &M4Macro, indent_level: usize) -> String {
        const TAB_WIDTH: usize = 2;
        let tab = " ".repeat(indent_level * TAB_WIDTH);
        format!(
            "{tab}{}({})",
            m4_macro.original_name.as_ref().unwrap_or(&m4_macro.name),
            m4_macro
                .args
                .iter()
                .map(|arg| self.m4_argument_to_string(arg, indent_level + 1))
                .collect::<Vec<String>>()
                .join(&format!(",\n{tab}  "))
        )
    }

    /// Format an M4 macro argument (`M4Argument`) into a string, applying `indent_level`
    /// spaces for multiline argument formatting.
    fn m4_argument_to_string(&self, arg: &M4Argument, indent_level: usize) -> String {
        use crate::m4_macro::M4Argument::*;
        const TAB_WIDTH: usize = 2;
        let newline = format!("\n{}", " ".repeat(indent_level * TAB_WIDTH));
        match arg {
            Literal(lit) => format!("[{}]", lit),
            Word(word) => self.display_word(word, false),
            Array(words) => format!(
                "{}",
                words
                    .iter()
                    .map(|w| self.display_word(w, false))
                    .collect::<Vec<String>>()
                    .join(if words.len() < 10 { " " } else { &newline })
            ),
            Program(prog) => format!("[{}]", prog.replace("\n", &newline)),
            Commands(cmds) => {
                if !cmds.is_empty() {
                    format!(
                        "[\n{}{newline}]",
                        cmds.iter()
                            .map(|c| self.display_node(*c, indent_level + 2))
                            .collect::<Vec<String>>()
                            .join("\n")
                    )
                } else {
                    "[]".to_owned()
                }
            }
            Condition(cond) => format!("[{}]", self.condition_to_string(cond)),
            Unknown(unknown) => format!("[{}]", unknown),
        }
    }
}

impl<U> NodePool<AcWord> for AutoconfPool<U> {}

/// A macro call utilizing M4 macros.
//Macro(M4Macro),

/// Trait providing access to a pool of AST nodes and methods to convert AST nodes
/// and components to their shell script string representations.
///
/// The type parameter `L` represents the literal type used in words and must implement
/// `fmt::Display` for formatting.
pub trait NodePool<W>: DisplayNode<Word = W> {
    /// Format the AST node with the given `node_id` into a string, indenting each line
    /// by `indent` spaces to represent nested structures.
    fn command_to_string(
        &self,
        cmd: &ShellCommand<W>,
        comment: Option<String>,
        indent_level: usize,
    ) -> String {
        use self::ShellCommand::*;
        const TAB_WIDTH: usize = 2;
        let tab = " ".repeat(indent_level * TAB_WIDTH);

        let content = match cmd {
            Assignment(name, word) => {
                format!("{tab}{}={}", name, self.display_word(word, true))
            }
            Cmd(words) => format!(
                "{tab}{}",
                words
                    .iter()
                    .enumerate()
                    .map(|(i, w)| self.display_word(w, i > 0))
                    .collect::<Vec<String>>()
                    .join(" ")
            ),
            Brace(cmds) => {
                format!(
                    "{tab}{{\n{}{tab}}}",
                    self.body_to_string(cmds, indent_level + 1)
                )
            }
            Subshell(cmds) => {
                format!(
                    "{tab}(\n{}{tab})",
                    self.body_to_string(cmds, indent_level + 1)
                )
            }
            While(pair) => format!(
                "{tab}while {}; do\n{}{tab}done",
                self.condition_to_string(&pair.condition),
                self.body_to_string(&pair.body, indent_level + 1)
            ),
            Until(pair) => format!(
                "{tab}until {}; do\n{}{tab}done",
                self.condition_to_string(&pair.condition),
                self.body_to_string(&pair.body, indent_level + 1)
            ),
            If {
                conditionals,
                else_branch,
            } => {
                let first_if = {
                    let pair = conditionals.first().unwrap();
                    format!(
                        "{tab}if {}; then\n{}",
                        self.condition_to_string(&pair.condition),
                        self.body_to_string(&pair.body, indent_level + 1)
                    )
                };
                let else_if = conditionals
                    .iter()
                    .skip(1)
                    .map(|pair| {
                        format!(
                            "{tab}else if {}; then\n{}",
                            self.condition_to_string(&pair.condition),
                            self.body_to_string(&pair.body, indent_level + 1)
                        )
                    })
                    .collect::<Vec<String>>()
                    .join("");
                let rest = if else_branch.is_empty() {
                    format!("{tab}fi")
                } else {
                    format!(
                        "{tab}else\n{}{tab}fi",
                        self.body_to_string(else_branch, indent_level + 1)
                    )
                };
                format!("{}{}{}", first_if, else_if, rest)
            }
            For { var, words, body } => format!(
                "{tab}for {var} in {}; do\n{}{tab}done",
                words
                    .iter()
                    .map(|w| self.display_word(w, true))
                    .collect::<Vec<String>>()
                    .join(" "),
                self.body_to_string(body, indent_level + 1)
            ),
            Case { word, arms } => format!(
                "{tab}case {} in\n{}{tab}esac",
                self.display_word(word, false),
                {
                    let mut arms = arms
                        .iter()
                        .map(|arm| {
                            format!(
                                "{tab}  {})\n{}{tab}    ;;",
                                arm.patterns
                                    .iter()
                                    .map(|w| self.display_word(w, false))
                                    .collect::<Vec<String>>()
                                    .join("|"),
                                self.body_to_string(&arm.body, indent_level + 2)
                            )
                        })
                        .collect::<Vec<String>>()
                        .join("\n");
                    if !arms.is_empty() {
                        arms.push('\n');
                    }
                    arms
                }
            ),
            And(cond, id) => format!(
                "{tab}{} && {}",
                self.condition_to_string(cond),
                self.display_node(*id, 0)
            ),
            Or(cond, id) => format!(
                "{tab}{} || {}",
                self.condition_to_string(cond),
                self.display_node(*id, 0)
            ),
            Pipe(bang, cmds) => format!(
                "{tab}{}{}",
                if *bang { "!" } else { "" },
                cmds.iter()
                    .map(|c| self.display_node(*c, 0))
                    .collect::<Vec<String>>()
                    .join(" | ")
            ),
            Redirect(cmd, redirects) => format!(
                "{} {}",
                self.display_node(*cmd, indent_level),
                redirects
                    .iter()
                    .map(|r| self.redirect_to_string(r))
                    .collect::<Vec<String>>()
                    .join(" ")
            ),
            Background(cmd) => format!("{tab}{} &", self.display_node(*cmd, indent_level)),
            FunctionDef { name, body } => {
                format!("function {name} () {}", self.display_node(*body, 0))
            }
        };
        if let Some(comment) = comment {
            let newline = format!("\n{tab}");
            format!("{tab}{}\n{}", comment.replace('\n', &newline), content)
        } else {
            content
        }
    }

    /// Convert a list of command node IDs to a formatted string.
    fn body_to_string(&self, cmds: &[NodeId], level: usize) -> String {
        let mut body = cmds
            .iter()
            .map(|c| self.display_node(*c, level))
            .filter(|s| !s.is_empty())
            .collect::<Vec<String>>()
            .join("\n");
        if !body.is_empty() {
            body.push('\n');
        }
        body
    }

    /// Format a `Condition` AST into its shell script string representation.
    fn condition_to_string(&self, cond: &Condition<W>) -> String {
        use super::condition::Condition::*;
        match cond {
            Cond(op) => format!("test {}", self.operator_to_string(op)),
            Not(cond) => format!("! {}", self.condition_to_string(cond)),
            And(lhs, rhs) => format!(
                "{} && {}",
                self.condition_to_string(lhs),
                self.condition_to_string(rhs)
            ),
            Or(lhs, rhs) => format!(
                "{} && {}",
                self.condition_to_string(lhs),
                self.condition_to_string(rhs)
            ),
            Eval(cmd) => format!("eval \"{}\"", cmd.to_string()),
            ReturnZero(cmd) => format!("{}", self.display_node(**cmd, 0)),
        }
    }

    /// Format a redirection AST (`Redirect`) into its shell script string representation,
    /// such as input/output redirections and here-documents.
    fn redirect_to_string(&self, redirect: &Redirect<W>) -> String {
        use super::Redirect::*;
        match redirect {
            Read(fd, file) => format!(
                "{}< {}",
                fd.map_or("".to_string(), |f| f.to_string()),
                self.display_word(file, false)
            ),
            Write(fd, file) => format!(
                "{}> {}",
                fd.map_or("".to_string(), |f| f.to_string()),
                self.display_word(file, false)
            ),
            ReadWrite(fd, file) => format!(
                "{}<> {}",
                fd.map_or("".to_string(), |f| f.to_string()),
                self.display_word(file, false)
            ),
            Append(fd, file) => format!(
                "{}>> {}",
                fd.map_or("".to_string(), |f| f.to_string()),
                self.display_word(file, false)
            ),
            Clobber(fd, file) => format!(
                "{}>| {}",
                fd.map_or("".to_string(), |f| f.to_string()),
                self.display_word(file, false)
            ),
            Heredoc(fd, file) => format!(
                "{}<<EOF\n{}EOF",
                fd.map_or("".to_string(), |f| f.to_string()),
                self.display_word(file, false)
            ),
            DupRead(fd, file) => format!(
                "{}<&{}",
                fd.map_or("".to_string(), |f| f.to_string()),
                self.display_word(file, false)
            ),
            DupWrite(fd, file) => format!(
                "{}>&{}",
                fd.map_or("".to_string(), |f| f.to_string()),
                self.display_word(file, false)
            ),
        }
    }

    /// Recursively format a `WordFragment` into its literal string form, handling quoting
    /// and parameter substitutions.
    fn shell_word_to_string(&self, shell_word: &WordFragment<W>) -> String {
        use crate::ast::minimal::WordFragment::*;
        match &shell_word {
            Literal(lit) => lit.to_owned(),
            DoubleQuoted(frags) => format!(
                "\"{}\"",
                frags
                    .iter()
                    .map(|frag| self.shell_word_to_string(frag))
                    .collect::<Vec<String>>()
                    .concat()
            ),
            Escaped(lit) => format!("\\{}", lit),
            Param(param) => format!("${{{}}}", param_raw(&param)),
            Subst(param_subst) => self.parameter_substitution_to_string(&param_subst),
            Star => "*".to_string(),
            Question => "?".to_string(),
            SquareOpen => "[".to_string(),
            SquareClose => "]".to_string(),
            Tilde => "~".to_string(),
            Colon => ":".to_string(),
        }
    }

    /// Format a parameter substitution AST (`ParameterSubstitution`) into its shell script string representation.
    fn parameter_substitution_to_string(&self, param_subst: &ParameterSubstitution<W>) -> String {
        use super::ParameterSubstitution::*;
        match param_subst {
            Command(cmds) => format!(
                "$({})",
                cmds.iter()
                    .map(|c| self.display_node(*c, 0))
                    .collect::<Vec<String>>()
                    .join(";")
            ),
            Len(param) => format!("${{#{}}}", param_raw(param)),
            Arith(arith) => format!(
                "$(( {} ))",
                arith.as_ref().map_or("".to_string(), |a| a.to_string())
            ),
            Default(_, param, word) => format!(
                "${{{}:-{}}}",
                param_raw(param),
                word.as_ref()
                    .map_or("".to_string(), |w| self.display_word(w, false))
            ),
            Assign(_, param, word) => format!(
                "${{{}:={}}}",
                param_raw(param),
                word.as_ref()
                    .map_or("".to_string(), |w| self.display_word(w, false))
            ),
            Error(_, param, word) => format!(
                "${{{}:?{}}}",
                param_raw(param),
                word.as_ref()
                    .map_or("".to_string(), |w| self.display_word(w, false))
            ),
            Alternative(_, param, word) => format!(
                "${{{}:+{}}}",
                param_raw(param),
                word.as_ref()
                    .map_or("".to_string(), |w| self.display_word(w, false))
            ),
            RemoveSmallestSuffix(param, pattern) => format!(
                "${{{}%{}}}",
                param_raw(param),
                pattern
                    .as_ref()
                    .map_or("".to_string(), |w| self.display_word(w, false))
            ),
            RemoveLargestSuffix(param, pattern) => format!(
                "${{{}%%{}}}",
                param_raw(param),
                pattern
                    .as_ref()
                    .map_or("".to_string(), |w| self.display_word(w, false))
            ),
            RemoveSmallestPrefix(param, pattern) => format!(
                "${{{}#{}}}",
                param_raw(param),
                pattern
                    .as_ref()
                    .map_or("".to_string(), |w| self.display_word(w, false))
            ),
            RemoveLargestPrefix(param, pattern) => format!(
                "${{{}##{}}}",
                param_raw(param),
                pattern
                    .as_ref()
                    .map_or("".to_string(), |w| self.display_word(w, false))
            ),
        }
    }

    /// Format an operator AST (`Operator`) into its shell test string representation
    /// (e.g., equality tests, file tests, and unary operators).
    fn operator_to_string(&self, op: &Operator<W>) -> String {
        use super::condition::Operator::*;
        match op {
            Eq(lhs, rhs) => format!(
                "{} = {}",
                self.display_word(lhs, false),
                self.display_word(rhs, false)
            ),
            Neq(lhs, rhs) => format!(
                "{} != {}",
                self.display_word(lhs, false),
                self.display_word(rhs, false)
            ),
            Ge(lhs, rhs) => format!(
                "{} -ge {}",
                self.display_word(lhs, false),
                self.display_word(rhs, false)
            ),
            Gt(lhs, rhs) => format!(
                "{} -gt {}",
                self.display_word(lhs, false),
                self.display_word(rhs, false)
            ),
            Le(lhs, rhs) => format!(
                "{} -le {}",
                self.display_word(lhs, false),
                self.display_word(rhs, false)
            ),
            Lt(lhs, rhs) => format!(
                "{} -lt {}",
                self.display_word(lhs, false),
                self.display_word(rhs, false)
            ),
            Empty(w) => format!("-z {}", self.display_word(w, false)),
            NonEmpty(w) => format!("-n {}", self.display_word(w, false)),
            Dir(w) => format!("-d {}", self.display_word(w, false)),
            File(w) => format!("-f {}", self.display_word(w, false)),
            NoExists(w) => format!("! -e {}", self.display_word(w, false)),
        }
    }
}

fn param_raw(param: &super::Parameter<String>) -> String {
    use super::Parameter::*;
    match param {
        At => "@".to_string(),
        Star => "*".to_string(),
        Pound => "#".to_string(),
        Question => "?".to_string(),
        Dash => "-".to_string(),
        Dollar => "$".to_string(),
        Bang => "!".to_string(),
        Positional(p) => p.to_string(),
        Var(v) => v.to_string(),
    }
}

#[cfg(test)]
mod tests {
    use super::{AcCommand, AcWord, AutoconfPool, Node, NodePool, ShellCommand};
    use crate::ast::minimal::Word;
    use crate::ast::minimal::WordFragment::*;
    use crate::ast::node::Condition;
    use crate::ast::node::DisplayNode;
    use crate::ast::node::GuardBodyPair;
    use crate::ast::node::Operator;
    use crate::ast::node::ParameterSubstitution;
    use crate::ast::node::PatternBodyPair;
    use crate::ast::node::Redirect;
    use crate::ast::MayM4::*;
    use crate::ast::Parameter;
    use crate::m4_macro::{M4Argument, M4Macro};

    type C = ShellCommand<AcWord>;

    #[test]
    fn test_get_node_kind() {
        let node = Node::new(None, None, AcCommand::new_cmd(C::Cmd(vec![])), None::<()>);
        let pool = AutoconfPool::from_vec(vec![node.clone()]);
        assert_eq!(pool.get(0).map(|n| &n.cmd), Some(&node.cmd));
    }

    #[test]
    fn test_node_to_string_assignment_and_cmd() {
        let word: AcWord = Word::Single(Shell(Literal("value".to_string()))).into();
        let assign_node = Node::new(
            None,
            None,
            AcCommand::new_cmd(C::Assignment("var".to_string(), word.clone().into())),
            (),
        );
        let echo: AcWord = Word::Single(Shell(Literal("echo".to_string()))).into();
        let cmd_node = Node::new(
            None,
            None,
            AcCommand::new_cmd(C::Cmd(vec![echo.clone(), word.clone()])),
            (),
        );
        let pool = AutoconfPool::from_vec(vec![assign_node, cmd_node]);
        assert_eq!(pool.display_node(0, 0), "var=\"value\"");
        assert_eq!(pool.display_node(1, 1), "  echo value");
    }

    #[test]
    fn test_operator_to_string() {
        let pool: AutoconfPool<()> = AutoconfPool::from_vec(vec![]);
        let op = Operator::Eq(
            Word::Single(Shell(Literal("a".to_string()))).into(),
            Word::Single(Shell(Literal("b".to_string()))).into(),
        );
        assert_eq!(pool.operator_to_string(&op), "a = b");
    }

    #[test]
    fn test_word_and_fragment_to_string() {
        let pool: AutoconfPool<()> = AutoconfPool::from_vec(vec![]);
        let frag = DoubleQuoted(vec![Literal("hi".to_string())]);
        assert_eq!(pool.shell_word_to_string(&frag), "\"hi\"");
        let word = Word::Concat(vec![
            Shell(Literal("a".to_string())),
            Shell(Literal("b".to_string())),
        ])
        .into();
        assert_eq!(pool.display_word(&word, false), "ab");
    }

    #[test]
    fn test_condition_to_string() {
        let pool: AutoconfPool<()> = AutoconfPool::from_vec(vec![]);
        let cond = Condition::Cond(Operator::Neq(
            Word::Single(Shell(Literal("x".to_string()))).into(),
            Word::Single(Shell(Literal("y".to_string()))).into(),
        ));
        assert_eq!(pool.condition_to_string(&cond), "test x != y");
    }

    #[test]
    fn test_redirect_to_string() {
        let pool: AutoconfPool<()> = AutoconfPool::from_vec(vec![]);
        let word = Word::Single(Shell(Literal("out".to_string()))).into();
        let redir = crate::ast::Redirect::Write(None, word);
        assert_eq!(pool.redirect_to_string(&redir), "> out");
    }

    #[test]
    fn test_parameter_substitution_to_string_default() {
        let pool: AutoconfPool<()> = AutoconfPool::from_vec(vec![]);
        let subst = ParameterSubstitution::Default(
            false,
            Parameter::Var("v".to_string()),
            Some(Word::Single(Shell(Literal("w".to_string()))).into()),
        );
        assert_eq!(pool.parameter_substitution_to_string(&subst), "${v:-w}");
    }

    #[test]
    fn test_m4_argument_to_string_literal() {
        let pool: AutoconfPool<()> = AutoconfPool::from_vec(vec![]);
        let arg = M4Argument::Literal("lit".to_string());
        assert_eq!(pool.m4_argument_to_string(&arg, 0), "[lit]");
    }

    #[test]
    fn test_node_to_string_brace_and_subshell() {
        let echo: AcWord = Word::Single(Shell(Literal("echo".to_string()))).into();
        let hi: AcWord = Word::Single(Shell(Literal("hi".to_string()))).into();
        let cmd = Node::new(
            None,
            None,
            AcCommand::new_cmd(C::Cmd(vec![echo.clone(), hi.clone()])),
            (),
        );
        let brace = Node::new(None, None, AcCommand::new_cmd(C::Brace(vec![0])), ());
        let subshell = Node::new(None, None, AcCommand::new_cmd(C::Subshell(vec![0])), ());
        let pool = AutoconfPool::from_vec(vec![cmd, brace, subshell]);
        assert_eq!(pool.display_node(1, 0), "{\n  echo hi\n}");
        assert_eq!(pool.display_node(2, 0), "(\n  echo hi\n)");
    }

    #[test]
    fn test_node_to_string_while_until() {
        let w: AcWord = Word::Single(Shell(Literal("f".to_string()))).into();
        let cond = Condition::Cond(Operator::Empty(w.clone()));
        let body = Node::new(None, None, AcCommand::new_cmd(C::Cmd(vec![w.clone()])), ());
        let gbp = GuardBodyPair {
            condition: cond.clone(),
            body: vec![0],
        };
        let while_node = Node::new(None, None, AcCommand::new_cmd(C::While(gbp.clone())), ());
        let until_node = Node::new(None, None, AcCommand::new_cmd(C::Until(gbp.clone())), ());
        let pool =
            AutoconfPool::from_vec(vec![body.clone(), while_node.clone(), until_node.clone()]);
        assert_eq!(pool.display_node(1, 0), "while test -z f; do\n  f\ndone");
        assert_eq!(pool.display_node(2, 0), "until test -z f; do\n  f\ndone");
    }

    #[test]
    fn test_node_to_string_if_else() {
        let w: AcWord = Word::Single(Shell(Literal("t".to_string()))).into();
        let cond = Condition::Cond(Operator::Empty(w.clone()));
        let cmd_true = Node::new(None, None, AcCommand::new_cmd(C::Cmd(vec![w.clone()])), ());
        let cmd_false = Node::new(
            None,
            None,
            AcCommand::new_cmd(C::Cmd(vec![
                Word::Single(Shell(Literal("f".to_string()))).into()
            ])),
            (),
        );
        let cmd_else = Node::new(
            None,
            None,
            AcCommand::new_cmd(C::Cmd(vec![
                Word::Single(Shell(Literal("e".to_string()))).into()
            ])),
            (),
        );
        let gbp1 = GuardBodyPair {
            condition: cond.clone(),
            body: vec![0],
        };
        let gbp2 = GuardBodyPair {
            condition: cond.clone(),
            body: vec![1],
        };
        let if_node = Node::new(
            None,
            None,
            AcCommand::new_cmd(C::If {
                conditionals: vec![gbp1.clone()],
                else_branch: vec![],
            }),
            (),
        );
        let if_else_node = Node::new(
            None,
            None,
            AcCommand::new_cmd(C::If {
                conditionals: vec![gbp1, gbp2],
                else_branch: vec![2],
            }),
            (),
        );
        let pool = AutoconfPool::from_vec(vec![
            cmd_true.clone(),
            cmd_false.clone(),
            cmd_else.clone(),
            if_node.clone(),
            if_else_node.clone(),
        ]);
        assert_eq!(pool.display_node(3, 0), "if test -z t; then\n  t\nfi");
        assert_eq!(
            pool.display_node(4, 0),
            "if test -z t; then\n  t\nelse if test -z t; then\n  f\nelse\n  e\nfi"
        );
    }

    #[test]
    fn test_node_to_string_for() {
        let cmd = Node::new(
            None,
            None,
            AcCommand::new_cmd(C::Cmd(vec![
                Word::Single(Shell(Literal("x".to_string()))).into()
            ])),
            (),
        );
        let words = vec![
            Word::Single(Shell(Literal("1".to_string()))).into(),
            Word::Single(Shell(Literal("2".to_string()))).into(),
        ];
        let for_node = Node::new(
            None,
            None,
            AcCommand::new_cmd(C::For {
                var: "i".to_string(),
                words: words.clone(),
                body: vec![0],
            }),
            (),
        );
        let pool = AutoconfPool::from_vec(vec![cmd.clone(), for_node.clone()]);
        assert_eq!(
            pool.display_node(1, 0),
            "for i in \"1\" \"2\"; do\n  x\ndone"
        );
    }

    #[test]
    fn test_node_to_string_case() {
        let cmd = Node::new(
            None,
            None,
            AcCommand::new_cmd(C::Cmd(vec![
                Word::Single(Shell(Literal("c".to_string()))).into()
            ])),
            (),
        );
        let pat1: AcWord = Word::Single(Shell(Literal("a".to_string()))).into();
        let pat2: AcWord = Word::Single(Shell(Literal("b".to_string()))).into();
        let arm = PatternBodyPair {
            comments: vec![],
            patterns: vec![pat1.clone(), pat2.clone()],
            body: vec![0],
        };
        let case_node = Node::new(
            None,
            None,
            AcCommand::new_cmd(C::Case {
                word: Word::Single(Shell(Literal("x".to_string()))).into(),
                arms: vec![arm],
            }),
            (),
        );
        let pool = AutoconfPool::from_vec(vec![cmd.clone(), case_node.clone()]);
        assert_eq!(
            pool.display_node(1, 0),
            "case x in\n  a|b)\n    c\n    ;;\nesac"
        );
    }

    #[test]
    fn test_node_to_string_and_or_pipe() {
        let cmd = Node::new(
            None,
            None,
            AcCommand::new_cmd(C::Cmd(vec![
                Word::Single(Shell(Literal("c".to_string()))).into()
            ])),
            (),
        );
        let eq = Operator::Eq(
            Word::Single(Shell(Literal("a".to_string()))).into(),
            Word::Single(Shell(Literal("b".to_string()))).into(),
        );
        let cond = Condition::Cond(eq.clone());
        let and_node = Node::new(None, None, AcCommand::new_cmd(C::And(cond.clone(), 0)), ());
        let or_node = Node::new(None, None, AcCommand::new_cmd(C::Or(cond.clone(), 0)), ());
        let pipe_node = Node::new(
            None,
            None,
            AcCommand::new_cmd(C::Pipe(false, vec![0, 0])),
            (),
        );
        let bang_pipe = Node::new(
            None,
            None,
            AcCommand::new_cmd(C::Pipe(true, vec![0, 0])),
            (),
        );
        let pool = AutoconfPool::from_vec(vec![
            cmd.clone(),
            and_node.clone(),
            or_node.clone(),
            pipe_node.clone(),
            bang_pipe.clone(),
        ]);
        assert_eq!(pool.display_node(1, 0), "test a = b && c");
        assert_eq!(pool.display_node(2, 0), "test a = b || c");
        assert_eq!(pool.display_node(3, 0), "c | c");
        assert_eq!(pool.display_node(4, 0), "!c | c");
    }

    #[test]
    fn test_node_to_string_redirect_and_background() {
        let cmd = Node::new(
            None,
            None,
            AcCommand::new_cmd(C::Cmd(vec![Word::Single(Shell(Literal(
                "cmd".to_string(),
            )))
            .into()])),
            (),
        );
        let r1 = Redirect::Read(None, Word::Single(Shell(Literal("in".to_string()))).into());
        let r2 = Redirect::Write(
            Some(1),
            Word::Single(Shell(Literal("out".to_string()))).into(),
        );
        let rd_node = Node::new(
            None,
            None,
            AcCommand::new_cmd(C::Redirect(0, vec![r1, r2])),
            (),
        );
        let bg_node = Node::new(None, None, AcCommand::new_cmd(C::Background(0)), ());
        let pool = AutoconfPool::from_vec(vec![cmd.clone(), rd_node.clone(), bg_node.clone()]);
        assert_eq!(pool.display_node(1, 0), "cmd < in 1> out");
        assert_eq!(pool.display_node(2, 0), "cmd &");
    }

    #[test]
    fn test_node_to_string_functiondef_and_macro() {
        let body = Node::new(
            None,
            None,
            AcCommand::new_cmd(C::Cmd(vec![
                Word::Single(Shell(Literal("b".to_string()))).into()
            ])),
            (),
        );
        let func = Node::new(
            None,
            None,
            AcCommand::new_cmd(C::FunctionDef {
                name: "f".to_string(),
                body: 0,
            }),
            (),
        );
        let m4 = M4Macro::new(
            "m".to_string(),
            vec![
                M4Argument::Literal("a".to_string()),
                M4Argument::Word(Word::Single(Shell(Literal("w".to_string()))).into()),
            ],
        );
        let mac = Node::new(None, None, m4.clone().into(), ());
        let pool = AutoconfPool::from_vec(vec![body.clone(), func.clone(), mac.clone()]);
        assert_eq!(pool.display_node(1, 0), "function f () b");
        assert_eq!(pool.display_node(2, 0), "m([a],\n  w)");
    }
}

fn add_newline(mut s: String) -> String {
    if s.is_empty() {
        s
    } else {
        s.push('\n');
        s
    }
}
