//! Defines node representations of the shell source.
use crate::m4_macro;

use super::{minimal::Word, Arithmetic};

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
    super::ParameterSubstitution<super::Parameter<String>, NodeId, W, Arithmetic<String>>;
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

/// Trait providing methods to convert AST nodes related to m4 macros to their script string
/// representations.
pub trait DisplayM4: NodePool<AcWord> {
    /// Standard indentation width for formatting M4 macros.
    const M4_TAB_WIDTH: usize = 2;
    /// Entry function for displaying a word fragment which can be a M4 macro call.
    fn display_may_m4_word(&self, frag: &AcWordFragment) -> String {
        self.may_m4_word_to_string(frag)
    }

    /// Entry function for displaying an M4 macro call (`M4Macro`).
    fn display_m4_macro(&self, m4_macro: &M4Macro, indent_level: usize) -> String {
        self.m4_macro_to_string(m4_macro, indent_level)
    }

    /// Entry function for displaying an M4 macro argument (`M4Argument`).
    fn display_m4_argument(&self, arg: &M4Argument, indent_level: usize) -> String {
        self.m4_argument_to_string(arg, indent_level)
    }

    /// Format a word fragment which is yet to turn out to be a M4 macro call
    fn may_m4_word_to_string(&self, frag: &AcWordFragment) -> String {
        use super::MayM4::*;
        match frag {
            Macro(macro_word) => format!(
                "{}({})",
                macro_word.name,
                macro_word
                    .args
                    .iter()
                    .map(|arg| self.display_m4_argument(arg, 0))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Shell(shell_word) => self.display_shell_word(shell_word),
        }
    }

    /// Format an M4 macro call (`M4Macro`) into a string
    fn m4_macro_to_string(&self, m4_macro: &M4Macro, indent_level: usize) -> String {
        let tab = " ".repeat(indent_level * Self::M4_TAB_WIDTH);
        format!(
            "{tab}{}({})",
            // m4_macro.original_name.as_ref().unwrap_or(&m4_macro.name),
            &m4_macro.name,
            m4_macro
                .args
                .iter()
                .map(|arg| self.display_m4_argument(arg, indent_level + 1))
                .collect::<Vec<String>>()
                .join(&format!(",\n{tab}  "))
        )
    }

    /// Format an M4 macro argument (`M4Argument`) into a string, applying `indent_level`
    /// spaces for multiline argument formatting.
    fn m4_argument_to_string(&self, arg: &M4Argument, indent_level: usize) -> String {
        use crate::m4_macro::M4Argument::*;
        let newline = format!("\n{}", " ".repeat(indent_level * Self::M4_TAB_WIDTH));
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
            Condition(cond) => format!("[{}]", self.display_condition(cond)),
            Unknown(unknown) => format!("[{}]", unknown),
        }
    }
}

/// A macro call utilizing M4 macros.
//Macro(M4Macro),

/// Trait providing access to a pool of AST nodes and methods to convert AST nodes
/// and components to their shell script string representations.
///
/// The type parameter `L` represents the literal type used in words and must implement
/// `fmt::Display` for formatting.
pub trait NodePool<W>: DisplayNode<Word = W> {
    /// Standard indentation width for formatting shell commands.
    const TAB_WIDTH: usize = 2;
    /// Entry function for displaying a command.
    fn display_command(
        &self,
        cmd: &ShellCommand<W>,
        comment: Option<String>,
        indent_level: usize,
    ) -> String {
        self.command_to_string(cmd, comment, indent_level)
    }

    /// Entry function for displaying a command body.
    fn display_body(&self, cmds: &[NodeId], level: usize) -> String {
        self.body_to_string(cmds, level)
    }

    /// Entry function for displaying a condition.
    fn display_condition(&self, cond: &Condition<W>) -> String {
        self.condition_to_string(cond)
    }

    /// Entry function for displaying a redirect.
    fn display_redirect(&self, redirect: &Redirect<W>) -> String {
        self.redirect_to_string(redirect)
    }

    /// Entry function for displaying a shell word.
    fn display_shell_word(&self, shell_word: &WordFragment<W>) -> String {
        self.shell_word_to_string(shell_word)
    }

    /// Entry function for displaying an inner parameter.
    fn display_inner_param(&self, param: &super::Parameter<String>) -> String {
        self.inner_param_to_string(param)
    }

    /// Entry function for displaying a parameter substitution.
    fn display_parameter_substitution(&self, param_subst: &ParameterSubstitution<W>) -> String {
        self.parameter_substitution_to_string(param_subst)
    }

    /// Entry function for displaying an operator.
    fn display_operator(&self, op: &Operator<W>) -> String {
        self.operator_to_string(op)
    }

    /// Entry function for displaying an assignment command.
    fn display_assignment(&self, name: &str, word: &W, indent_level: usize) -> String {
        self.assignment_to_string(name, word, indent_level)
    }

    /// Entry function for displaying a simple command.
    fn display_cmd(&self, words: &[W], indent_level: usize) -> String {
        self.cmd_to_string(words, indent_level)
    }

    /// Entry function for displaying a brace command.
    fn display_brace(&self, cmds: &[NodeId], indent_level: usize) -> String {
        self.brace_to_string(cmds, indent_level)
    }

    /// Entry function for displaying a subshell command.
    fn display_subshell(&self, cmds: &[NodeId], indent_level: usize) -> String {
        self.subshell_to_string(cmds, indent_level)
    }

    /// Entry function for displaying a while loop.
    fn display_while(&self, pair: &GuardBodyPair<W>, indent_level: usize) -> String {
        self.while_to_string(pair, indent_level)
    }

    /// Entry function for displaying an until loop.
    fn display_until(&self, pair: &GuardBodyPair<W>, indent_level: usize) -> String {
        self.until_to_string(pair, indent_level)
    }

    /// Entry function for displaying an if statement.
    fn display_if(
        &self,
        conditionals: &[GuardBodyPair<W>],
        else_branch: &[NodeId],
        indent_level: usize,
    ) -> String {
        self.if_to_string(conditionals, else_branch, indent_level)
    }

    /// Entry function for displaying a for loop.
    fn display_for(&self, var: &str, words: &[W], body: &[NodeId], indent_level: usize) -> String {
        self.for_to_string(var, words, body, indent_level)
    }

    /// Entry function for displaying a case statement.
    fn display_case(&self, word: &W, arms: &[PatternBodyPair<W>], indent_level: usize) -> String {
        self.case_to_string(word, arms, indent_level)
    }

    /// Entry function for displaying an and command.
    fn display_and(&self, cond: &Condition<W>, id: NodeId, indent_level: usize) -> String {
        self.and_to_string(cond, id, indent_level)
    }

    /// Entry function for displaying an or command.
    fn display_or(&self, cond: &Condition<W>, id: NodeId, indent_level: usize) -> String {
        self.or_to_string(cond, id, indent_level)
    }

    /// Entry function for displaying a pipe command.
    fn display_pipe(&self, bang: bool, cmds: &[NodeId], indent_level: usize) -> String {
        self.pipe_to_string(bang, cmds, indent_level)
    }

    /// Entry function for displaying a redirect command.
    fn display_redirect_cmd(
        &self,
        cmd: NodeId,
        redirects: &[Redirect<W>],
        indent_level: usize,
    ) -> String {
        self.redirect_cmd_to_string(cmd, redirects, indent_level)
    }

    /// Entry function for displaying a background command.
    fn display_background(&self, cmd: NodeId, indent_level: usize) -> String {
        self.background_to_string(cmd, indent_level)
    }

    /// Entry function for displaying a function definition.
    fn display_function_def(&self, name: &str, body: NodeId) -> String {
        self.function_def_to_string(name, body)
    }

    /// Entry function for displaying an arithmetic.
    fn display_arithmetic(&self, arith: &Arithmetic<String>) -> String {
        arith.to_string()
    }

    /// Format the AST node with the given `node_id` into a string, indenting each line
    /// by `indent` spaces to represent nested structures.
    fn command_to_string(
        &self,
        cmd: &ShellCommand<W>,
        comment: Option<String>,
        indent_level: usize,
    ) -> String {
        use self::ShellCommand::*;
        let tab = " ".repeat(indent_level * Self::TAB_WIDTH);

        let content = match cmd {
            Assignment(name, word) => self.display_assignment(name, word, indent_level),
            Cmd(words) => self.display_cmd(words, indent_level),
            Brace(cmds) => self.display_brace(cmds, indent_level),
            Subshell(cmds) => self.display_subshell(cmds, indent_level),
            While(pair) => self.display_while(pair, indent_level),
            Until(pair) => self.display_until(pair, indent_level),
            If {
                conditionals,
                else_branch,
            } => self.display_if(conditionals, else_branch, indent_level),
            For { var, words, body } => self.display_for(var, words, body, indent_level),
            Case { word, arms } => self.display_case(word, arms, indent_level),
            And(cond, id) => self.display_and(cond, *id, indent_level),
            Or(cond, id) => self.display_or(cond, *id, indent_level),
            Pipe(bang, cmds) => self.display_pipe(*bang, cmds, indent_level),
            Redirect(cmd, redirects) => self.display_redirect_cmd(*cmd, redirects, indent_level),
            Background(cmd) => self.display_background(*cmd, indent_level),
            FunctionDef { name, body } => self.display_function_def(name, *body),
        };
        if let Some(comment) = comment {
            let newline = format!("\n{tab}");
            format!("{tab}{}\n{}", comment.replace('\n', &newline), content)
        } else {
            content
        }
    }

    /// Format an assignment command into a string.
    fn assignment_to_string(&self, name: &str, word: &W, indent_level: usize) -> String {
        let tab = " ".repeat(indent_level * Self::TAB_WIDTH);
        format!("{tab}{}={}", name, self.display_word(word, true))
    }

    /// Format a simple command into a string.
    fn cmd_to_string(&self, words: &[W], indent_level: usize) -> String {
        let tab = " ".repeat(indent_level * Self::TAB_WIDTH);
        format!(
            "{tab}{}",
            words
                .iter()
                .enumerate()
                .map(|(i, w)| self.display_word(w, i > 0))
                .collect::<Vec<String>>()
                .join(" ")
        )
    }

    /// Format a brace command into a string.
    fn brace_to_string(&self, cmds: &[NodeId], indent_level: usize) -> String {
        let tab = " ".repeat(indent_level * Self::TAB_WIDTH);
        format!(
            "{tab}{{\n{}{tab}}}",
            self.display_body(cmds, indent_level + 1)
        )
    }

    /// Format a subshell command into a string.
    fn subshell_to_string(&self, cmds: &[NodeId], indent_level: usize) -> String {
        let tab = " ".repeat(indent_level * Self::TAB_WIDTH);
        format!(
            "{tab}(\n{}{tab})",
            self.display_body(cmds, indent_level + 1)
        )
    }

    /// Format a while loop into a string.
    fn while_to_string(&self, pair: &GuardBodyPair<W>, indent_level: usize) -> String {
        let tab = " ".repeat(indent_level * Self::TAB_WIDTH);
        format!(
            "{tab}while {}; do\n{}{tab}done",
            self.display_condition(&pair.condition),
            self.display_body(&pair.body, indent_level + 1)
        )
    }

    /// Format an until loop into a string.
    fn until_to_string(&self, pair: &GuardBodyPair<W>, indent_level: usize) -> String {
        let tab = " ".repeat(indent_level * Self::TAB_WIDTH);
        format!(
            "{tab}until {}; do\n{}{tab}done",
            self.display_condition(&pair.condition),
            self.display_body(&pair.body, indent_level + 1)
        )
    }

    /// Format an if statement into a string.
    fn if_to_string(
        &self,
        conditionals: &[GuardBodyPair<W>],
        else_branch: &[NodeId],
        indent_level: usize,
    ) -> String {
        let tab = " ".repeat(indent_level * Self::TAB_WIDTH);

        let first_if = {
            let pair = conditionals.first().unwrap();
            format!(
                "{tab}if {}; then\n{}",
                self.display_condition(&pair.condition),
                self.display_body(&pair.body, indent_level + 1)
            )
        };
        let else_if = conditionals
            .iter()
            .skip(1)
            .map(|pair| {
                format!(
                    "{tab}else if {}; then\n{}",
                    self.display_condition(&pair.condition),
                    self.display_body(&pair.body, indent_level + 1)
                )
            })
            .collect::<Vec<String>>()
            .join("");
        let rest = if else_branch.is_empty() {
            format!("{tab}fi")
        } else {
            format!(
                "{tab}else\n{}{tab}fi",
                self.display_body(else_branch, indent_level + 1)
            )
        };
        format!("{}{}{}", first_if, else_if, rest)
    }

    /// Format a for loop into a string.
    fn for_to_string(
        &self,
        var: &str,
        words: &[W],
        body: &[NodeId],
        indent_level: usize,
    ) -> String {
        let tab = " ".repeat(indent_level * Self::TAB_WIDTH);
        format!(
            "{tab}for {var} in {}; do\n{}{tab}done",
            words
                .iter()
                .map(|w| self.display_word(w, true))
                .collect::<Vec<String>>()
                .join(" "),
            self.display_body(body, indent_level + 1)
        )
    }

    /// Format a case statement into a string.
    fn case_to_string(&self, word: &W, arms: &[PatternBodyPair<W>], indent_level: usize) -> String {
        let tab = " ".repeat(indent_level * Self::TAB_WIDTH);
        format!(
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
                            self.display_body(&arm.body, indent_level + 2)
                        )
                    })
                    .collect::<Vec<String>>()
                    .join("\n");
                if !arms.is_empty() {
                    arms.push('\n');
                }
                arms
            }
        )
    }

    /// Format an and command into a string.
    fn and_to_string(&self, cond: &Condition<W>, id: NodeId, indent_level: usize) -> String {
        let tab = " ".repeat(indent_level * Self::TAB_WIDTH);
        format!(
            "{tab}{} && {}",
            self.display_condition(cond),
            self.display_node(id, 0)
        )
    }

    /// Format an or command into a string.
    fn or_to_string(&self, cond: &Condition<W>, id: NodeId, indent_level: usize) -> String {
        let tab = " ".repeat(indent_level * Self::TAB_WIDTH);
        format!(
            "{tab}{} || {}",
            self.display_condition(cond),
            self.display_node(id, 0)
        )
    }

    /// Format a pipe command into a string.
    fn pipe_to_string(&self, bang: bool, cmds: &[NodeId], indent_level: usize) -> String {
        let tab = " ".repeat(indent_level * Self::TAB_WIDTH);
        format!(
            "{tab}{}{}",
            if bang { "!" } else { "" },
            cmds.iter()
                .map(|c| self.display_node(*c, 0))
                .collect::<Vec<String>>()
                .join(" | ")
        )
    }

    /// Format a redirect command into a string.
    fn redirect_cmd_to_string(
        &self,
        cmd: NodeId,
        redirects: &[Redirect<W>],
        indent_level: usize,
    ) -> String {
        format!(
            "{} {}",
            self.display_node(cmd, indent_level),
            redirects
                .iter()
                .map(|r| self.display_redirect(r))
                .collect::<Vec<String>>()
                .join(" ")
        )
    }

    /// Format a background command into a string.
    fn background_to_string(&self, cmd: NodeId, indent_level: usize) -> String {
        let tab = " ".repeat(indent_level * Self::TAB_WIDTH);
        format!("{tab}{} &", self.display_node(cmd, indent_level))
    }

    /// Format a function definition into a string.
    fn function_def_to_string(&self, name: &str, body: NodeId) -> String {
        format!("function {name} () {}", self.display_node(body, 0))
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
            Cond(op) => format!("test {}", self.display_operator(op)),
            Not(cond) => format!("! {}", self.display_condition(cond)),
            And(lhs, rhs) => format!(
                "{} && {}",
                self.display_condition(lhs),
                self.display_condition(rhs)
            ),
            Or(lhs, rhs) => format!(
                "{} && {}",
                self.display_condition(lhs),
                self.display_condition(rhs)
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
                "{}",
                frags
                    .iter()
                    .map(|frag| self.display_shell_word(frag))
                    .collect::<Vec<String>>()
                    .concat()
            ),
            Escaped(lit) => format!("\\{}", lit),
            Param(param) => format!("${{{}}}", self.display_inner_param(&param)),
            Subst(param_subst) => self.display_parameter_substitution(&param_subst),
            Star => "*".to_string(),
            Question => "?".to_string(),
            SquareOpen => "[".to_string(),
            SquareClose => "]".to_string(),
            Tilde => "~".to_string(),
            Colon => ":".to_string(),
        }
    }

    /// Format a `Parameter` into its literal string form, except its prefixes such as '$'.
    fn inner_param_to_string(&self, param: &super::Parameter<String>) -> String {
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
            Len(param) => format!("${{#{}}}", self.display_inner_param(param)),
            Arith(arith) => format!(
                "$(( {} ))",
                arith
                    .as_ref()
                    .map_or("".to_string(), |a| self.display_arithmetic(a))
            ),
            Default(_, param, word) => format!(
                "${{{}:-{}}}",
                self.display_inner_param(param),
                word.as_ref()
                    .map_or("".to_string(), |w| self.display_word(w, false))
            ),
            Assign(_, param, word) => format!(
                "${{{}:={}}}",
                self.display_inner_param(param),
                word.as_ref()
                    .map_or("".to_string(), |w| self.display_word(w, false))
            ),
            Error(_, param, word) => format!(
                "${{{}:?{}}}",
                self.display_inner_param(param),
                word.as_ref()
                    .map_or("".to_string(), |w| self.display_word(w, false))
            ),
            Alternative(_, param, word) => format!(
                "${{{}:+{}}}",
                self.display_inner_param(param),
                word.as_ref()
                    .map_or("".to_string(), |w| self.display_word(w, false))
            ),
            RemoveSmallestSuffix(param, pattern) => format!(
                "${{{}%{}}}",
                self.display_inner_param(param),
                pattern
                    .as_ref()
                    .map_or("".to_string(), |w| self.display_word(w, false))
            ),
            RemoveLargestSuffix(param, pattern) => format!(
                "${{{}%%{}}}",
                self.display_inner_param(param),
                pattern
                    .as_ref()
                    .map_or("".to_string(), |w| self.display_word(w, false))
            ),
            RemoveSmallestPrefix(param, pattern) => format!(
                "${{{}#{}}}",
                self.display_inner_param(param),
                pattern
                    .as_ref()
                    .map_or("".to_string(), |w| self.display_word(w, false))
            ),
            RemoveLargestPrefix(param, pattern) => format!(
                "${{{}##{}}}",
                self.display_inner_param(param),
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
