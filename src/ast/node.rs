//! Defines node representations of the shell source.
use crate::m4_macro;
use std::fmt;

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

/// Trait providing access to a pool of AST nodes and methods to convert AST nodes
/// and components to their shell script string representations.
///
/// The type parameter `L` represents the literal type used in words and must implement
/// `fmt::Display` for formatting.
pub trait NodePool<L>
where
    L: fmt::Display + fmt::Debug,
{
    /// Retrieve an optional reference to the AST `Node` identified by `node_id`.
    /// If None, tracking is skipped. `nest_level` is fed as a supplemental info.
    fn get_node_kind(&self, node_id: NodeId, nest_level: usize) -> Option<&NodeKind<L>>;

    /// Format the AST node with the given `node_id` into a string, indenting each line
    /// by `indent` spaces to represent nested structures.
    fn node_to_string(&self, node_id: NodeId, nest_level: usize) -> String {
        use self::NodeKind::*;
        const TAB_WIDTH: usize = 2;
        let tab = " ".repeat(nest_level * TAB_WIDTH);

        let body_to_string = |cmds: &[NodeId], n| {
            cmds.iter()
                .map(|c| self.node_to_string(*c, n))
                .collect::<Vec<String>>()
                .join("\n")
        };

        if let Some(kind) = self.get_node_kind(node_id, nest_level) {
            match kind {
                Assignment(name, word) => {
                    format!("{tab}{}={}", name, self.word_to_string(word, true))
                }
                Cmd(words) => format!(
                    "{tab}{}",
                    words
                        .iter()
                        .map(|w| self.word_to_string(w, false))
                        .collect::<Vec<String>>()
                        .join(" ")
                ),
                Brace(cmds) => {
                    format!("{tab}{{\n{}\n{tab}}}", body_to_string(cmds, nest_level + 1))
                }
                Subshell(cmds) => {
                    format!("{tab}(\n{}\n{tab})", body_to_string(cmds, nest_level + 1))
                }
                While(pair) => format!(
                    "{tab}while {}; do\n{}\n{tab}done",
                    self.condition_to_string(&pair.condition),
                    body_to_string(&pair.body, nest_level + 1)
                ),
                Until(pair) => format!(
                    "{tab}until {}; do\n{}\n{tab}done",
                    self.condition_to_string(&pair.condition),
                    body_to_string(&pair.body, nest_level + 1)
                ),
                If {
                    conditionals,
                    else_branch,
                } => {
                    let first_if = {
                        let pair = conditionals.first().unwrap();
                        format!(
                            "{tab}if {}; then\n{}\n",
                            self.condition_to_string(&pair.condition),
                            body_to_string(&pair.body, nest_level + 1)
                        )
                    };
                    let else_if = conditionals
                        .iter()
                        .skip(1)
                        .map(|pair| {
                            format!(
                                "{tab}else if {}; then\n{}\n",
                                self.condition_to_string(&pair.condition),
                                body_to_string(&pair.body, nest_level + 1)
                            )
                        })
                        .collect::<Vec<String>>()
                        .join("");
                    let rest = if else_branch.is_empty() {
                        format!("{tab}fi")
                    } else {
                        format!(
                            "{tab}else\n{}\n{tab}fi",
                            body_to_string(else_branch, nest_level + 1)
                        )
                    };
                    format!("{}{}{}", first_if, else_if, rest)
                }
                For { var, words, body } => format!(
                    "{tab}for {var} in {}; do\n{}\n{tab}done",
                    words
                        .iter()
                        .map(|w| self.word_to_string(w, true))
                        .collect::<Vec<String>>()
                        .join(" "),
                    body_to_string(body, nest_level + 1)
                ),
                Case { word, arms } => format!(
                    "{tab}case {} in\n{}\n{tab}esac",
                    self.word_to_string(word, false),
                    arms.iter()
                        .map(|arm| format!(
                            "{tab}  {})\n{}\n{tab}    ;;",
                            arm.patterns
                                .iter()
                                .map(|w| self.word_to_string(w, false))
                                .collect::<Vec<String>>()
                                .join("|"),
                            body_to_string(&arm.body, nest_level + 1)
                        ))
                        .collect::<Vec<String>>()
                        .join("\n")
                ),
                And(cond, cmd) => format!(
                    "{} && {}",
                    self.condition_to_string(cond),
                    self.node_to_string(*cmd, nest_level)
                ),
                Or(cond, cmd) => format!(
                    "{} || {}",
                    self.condition_to_string(cond),
                    self.node_to_string(*cmd, nest_level)
                ),
                Pipe(bang, cmds) => format!(
                    "{}{}",
                    if *bang { "!" } else { "" },
                    cmds.iter()
                        .map(|c| self.node_to_string(*c, 0))
                        .collect::<Vec<String>>()
                        .join(" | ")
                ),
                Redirect(cmd, redirects) => format!(
                    "{} {}",
                    self.node_to_string(*cmd, nest_level),
                    redirects
                        .iter()
                        .map(|r| self.redirect_to_string(r))
                        .collect::<Vec<String>>()
                        .join(" ")
                ),
                Background(cmd) => format!("{} &", self.node_to_string(*cmd, nest_level)),
                FunctionDef { name, body } => {
                    format!("function {name} () {}", self.node_to_string(*body, 0))
                }
                Macro(m4_macro) => format!(
                    "{tab}{}({})",
                    m4_macro.original_name.as_ref().unwrap_or(&m4_macro.name),
                    m4_macro
                        .args
                        .iter()
                        .map(|arg| self.m4_argument_to_string(arg, nest_level + 1))
                        .collect::<Vec<String>>()
                        .join(&format!(",\n{tab}  "))
                ),
            }
        } else {
            // if node was not found, empty string is returned.
            String::new()
        }
    }

    /// Format a `Condition` AST into its shell script string representation.
    fn condition_to_string(&self, cond: &Condition<L>) -> String {
        use super::minimal::Condition::*;
        match cond {
            Cond(op) => format!("test {}", self.operator_to_string(op)),
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
            Eval(cmds) => format!(
                "eval \"{}\"",
                cmds.iter()
                    .map(|c| self.node_to_string(*c, 0))
                    .collect::<Vec<String>>()
                    .join("; ")
            ),
            ReturnZero(cmd) => format!("{}", self.node_to_string(**cmd, 0)),
        }
    }

    /// Format an M4 macro argument (`M4Argument`) into a string, applying `indent`
    /// spaces for multiline argument formatting.
    fn m4_argument_to_string(&self, arg: &M4Argument<L>, indent: usize) -> String {
        use crate::m4_macro::M4Argument::*;
        let newline = format!("\n{}", " ".repeat(indent));
        match arg {
            Literal(lit) => format!("[{}]", lit),
            Word(word) => self.word_to_string(word, false),
            Array(words) => format!(
                "{}",
                words
                    .iter()
                    .map(|w| self.word_to_string(w, false))
                    .collect::<Vec<String>>()
                    .join(if words.len() < 10 { " " } else { &newline })
            ),
            Program(prog) => format!("[{}]", prog.replace("\n", &newline)),
            Commands(cmds) => format!(
                "[\n{}{newline}]",
                cmds.iter()
                    .map(|c| self.node_to_string(*c, indent + 2))
                    .collect::<Vec<String>>()
                    .join("\n")
            ),
            Unknown(unknown) => format!("[{}]", unknown),
        }
    }

    /// Format a redirection AST (`Redirect`) into its shell script string representation,
    /// such as input/output redirections and here-documents.
    fn redirect_to_string(&self, redirect: &Redirect<L>) -> String {
        use super::Redirect::*;
        match redirect {
            Read(fd, file) => format!(
                "{}< {}",
                fd.map_or("".to_string(), |f| f.to_string()),
                self.word_to_string(file, false)
            ),
            Write(fd, file) => format!(
                "{}> {}",
                fd.map_or("".to_string(), |f| f.to_string()),
                self.word_to_string(file, false)
            ),
            ReadWrite(fd, file) => format!(
                "{}<> {}",
                fd.map_or("".to_string(), |f| f.to_string()),
                self.word_to_string(file, false)
            ),
            Append(fd, file) => format!(
                "{}>> {}",
                fd.map_or("".to_string(), |f| f.to_string()),
                self.word_to_string(file, false)
            ),
            Clobber(fd, file) => format!(
                "{}>| {}",
                fd.map_or("".to_string(), |f| f.to_string()),
                self.word_to_string(file, false)
            ),
            Heredoc(fd, file) => format!(
                "{}<<EOF\n{}EOF",
                fd.map_or("".to_string(), |f| f.to_string()),
                self.word_to_string(file, false)
            ),
            DupRead(fd, file) => format!(
                "{}<&{}",
                fd.map_or("".to_string(), |f| f.to_string()),
                self.word_to_string(file, false)
            ),
            DupWrite(fd, file) => format!(
                "{}>&{}",
                fd.map_or("".to_string(), |f| f.to_string()),
                self.word_to_string(file, false)
            ),
        }
    }

    /// Recursively format a `WordFragment` into its literal string form, handling quoting
    /// and parameter substitutions.
    fn word_fragment_to_string(&self, frag: &WordFragment<L>) -> String {
        use crate::ast::minimal::WordFragment::*;
        match frag {
            Literal(lit) => lit.to_string(),
            DoubleQuoted(frags) => format!(
                "\"{}\"",
                frags
                    .iter()
                    .map(|frag| self.word_fragment_to_string(frag))
                    .collect::<Vec<String>>()
                    .concat()
            ),
            Escaped(lit) => format!("\\{}", lit),
            Param(param) => param.to_string(),
            Subst(param_subst) => self.parameter_substitution_to_string(&param_subst),
            Star => "*".to_string(),
            Question => "?".to_string(),
            SquareOpen => "[".to_string(),
            SquareClose => "]".to_string(),
            Tilde => "~".to_string(),
            Colon => ":".to_string(),
            Macro(m4_macro) => format!(
                "{}({})",
                m4_macro.name,
                m4_macro
                    .args
                    .iter()
                    .map(|arg| self.m4_argument_to_string(arg, 0))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
        }
    }

    /// Format a complete `Word` (which may consist of fragments) into its shell script string form.
    fn word_to_string(&self, word: &Word<L>, quote: bool) -> String {
        use super::minimal::Word::*;
        use super::minimal::WordFragment::DoubleQuoted;
        match word {
            Empty => "\"\"".to_string(),
            Concat(frags) => {
                format!(
                    "{}",
                    frags
                        .iter()
                        .map(|w| self.word_fragment_to_string(w))
                        .collect::<Vec<String>>()
                        .concat()
                )
            }
            Single(frag) => {
                let s = self.word_fragment_to_string(frag);
                if quote && !matches!(frag, DoubleQuoted(_)) {
                    format!("\"{}\"", s)
                } else {
                    s
                }
            }
        }
    }

    /// Format a parameter substitution AST (`ParameterSubstitution`) into its shell script string representation.
    fn parameter_substitution_to_string(&self, param_subst: &ParameterSubstitution<L>) -> String {
        use super::Parameter::*;
        use super::ParameterSubstitution::*;
        let param_raw = |param: &super::Parameter<L>| match param {
            At => "@".to_string(),
            Star => "*".to_string(),
            Pound => "#".to_string(),
            Question => "?".to_string(),
            Dash => "-".to_string(),
            Dollar => "$".to_string(),
            Bang => "!".to_string(),
            Positional(p) => p.to_string(),
            Var(v) => v.to_string(),
        };
        match param_subst {
            Command(cmds) => format!(
                "$({})",
                cmds.iter()
                    .map(|c| self.node_to_string(*c, 0))
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
                    .map_or("".to_string(), |w| self.word_to_string(w, false))
            ),
            Assign(_, param, word) => format!(
                "${{{}:={}}}",
                param_raw(param),
                word.as_ref()
                    .map_or("".to_string(), |w| self.word_to_string(w, false))
            ),
            Error(_, param, word) => format!(
                "${{{}:?{}}}",
                param_raw(param),
                word.as_ref()
                    .map_or("".to_string(), |w| self.word_to_string(w, false))
            ),
            Alternative(_, param, word) => format!(
                "${{{}:+{}}}",
                param_raw(param),
                word.as_ref()
                    .map_or("".to_string(), |w| self.word_to_string(w, false))
            ),
            RemoveSmallestSuffix(param, pattern) => format!(
                "${{{}%{}}}",
                param_raw(param),
                pattern
                    .as_ref()
                    .map_or("".to_string(), |w| self.word_to_string(w, false))
            ),
            RemoveLargestSuffix(param, pattern) => format!(
                "${{{}%%{}}}",
                param_raw(param),
                pattern
                    .as_ref()
                    .map_or("".to_string(), |w| self.word_to_string(w, false))
            ),
            RemoveSmallestPrefix(param, pattern) => format!(
                "${{{}#{}}}",
                param_raw(param),
                pattern
                    .as_ref()
                    .map_or("".to_string(), |w| self.word_to_string(w, false))
            ),
            RemoveLargestPrefix(param, pattern) => format!(
                "${{{}##{}}}",
                param_raw(param),
                pattern
                    .as_ref()
                    .map_or("".to_string(), |w| self.word_to_string(w, false))
            ),
        }
    }

    /// Format an operator AST (`Operator`) into its shell test string representation
    /// (e.g., equality tests, file tests, and unary operators).
    fn operator_to_string(&self, op: &Operator<L>) -> String {
        use super::minimal::Operator::*;
        match op {
            Eq(lhs, rhs) => format!(
                "{} = {}",
                self.word_to_string(lhs, false),
                self.word_to_string(rhs, false)
            ),
            Neq(lhs, rhs) => format!(
                "{} != {}",
                self.word_to_string(lhs, false),
                self.word_to_string(rhs, false)
            ),
            Ge(lhs, rhs) => format!(
                "{} -ge {}",
                self.word_to_string(lhs, false),
                self.word_to_string(rhs, false)
            ),
            Gt(lhs, rhs) => format!(
                "{} -gt {}",
                self.word_to_string(lhs, false),
                self.word_to_string(rhs, false)
            ),
            Le(lhs, rhs) => format!(
                "{} -le {}",
                self.word_to_string(lhs, false),
                self.word_to_string(rhs, false)
            ),
            Lt(lhs, rhs) => format!(
                "{} -lt {}",
                self.word_to_string(lhs, false),
                self.word_to_string(rhs, false)
            ),
            Empty(w) => format!("-z {}", self.word_to_string(w, false)),
            NonEmpty(w) => format!("-n {}", self.word_to_string(w, false)),
            Dir(w) => format!("-d {}", self.word_to_string(w, false)),
            File(w) => format!("-f {}", self.word_to_string(w, false)),
            NoExists(w) => format!("! -e {}", self.word_to_string(w, false)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Parameter;
    use crate::m4_macro::{M4Argument, M4Macro};

    struct DummyPool {
        nodes: Vec<Node<String>>,
    }

    impl DummyPool {
        fn new(nodes: Vec<Node<String>>) -> Self {
            DummyPool { nodes }
        }
    }

    impl NodePool<String> for DummyPool {
        fn get_node_kind(&self, node_id: NodeId, _: usize) -> Option<&NodeKind<String>> {
            Some(&self.nodes[node_id].kind)
        }
    }

    #[test]
    fn test_get_node_kind() {
        let node = Node::new(None, None, NodeKind::<String>::Cmd(vec![]));
        let pool = DummyPool::new(vec![node.clone()]);
        assert_eq!(pool.get_node_kind(0, 0), Some(&node.kind));
    }

    #[test]
    fn test_node_to_string_assignment_and_cmd() {
        let word = Word::Single(WordFragment::Literal("value".to_string()));
        let assign_node = Node::new(
            None,
            None,
            NodeKind::Assignment("var".to_string(), word.clone()),
        );
        let echo = Word::Single(WordFragment::Literal("echo".to_string()));
        let cmd_node = Node::new(None, None, NodeKind::Cmd(vec![echo.clone(), word.clone()]));
        let pool = DummyPool::new(vec![assign_node, cmd_node]);
        assert_eq!(pool.node_to_string(0, 0), "var=\"value\"");
        assert_eq!(pool.node_to_string(1, 2), "  echo value");
    }

    #[test]
    fn test_operator_to_string() {
        let pool: DummyPool = DummyPool::new(vec![]);
        let op = Operator::Eq(
            Word::Single(WordFragment::Literal("a".to_string())),
            Word::Single(WordFragment::Literal("b".to_string())),
        );
        assert_eq!(pool.operator_to_string(&op), "a = b");
    }

    #[test]
    fn test_word_and_fragment_to_string() {
        let pool: DummyPool = DummyPool::new(vec![]);
        let frag = WordFragment::DoubleQuoted(vec![WordFragment::Literal("hi".to_string())]);
        assert_eq!(pool.word_fragment_to_string(&frag), "\"hi\"");
        let word = Word::Concat(vec![
            WordFragment::Literal("a".to_string()),
            WordFragment::Literal("b".to_string()),
        ]);
        assert_eq!(pool.word_to_string(&word, false), "ab");
    }

    #[test]
    fn test_condition_to_string() {
        let pool: DummyPool = DummyPool::new(vec![]);
        let cond = Condition::Cond(Operator::Neq(
            Word::Single(WordFragment::Literal("x".to_string())),
            Word::Single(WordFragment::Literal("y".to_string())),
        ));
        assert_eq!(pool.condition_to_string(&cond), "test x != y");
    }

    #[test]
    fn test_redirect_to_string() {
        let pool: DummyPool = DummyPool::new(vec![]);
        let redir = Redirect::Write(None, Word::Single(WordFragment::Literal("out".to_string())));
        assert_eq!(pool.redirect_to_string(&redir), "> out");
    }

    #[test]
    fn test_parameter_substitution_to_string_default() {
        let pool: DummyPool = DummyPool::new(vec![]);
        let subst = ParameterSubstitution::Default(
            false,
            Parameter::Var("v".to_string()),
            Some(Word::Single(WordFragment::Literal("w".to_string()))),
        );
        assert_eq!(pool.parameter_substitution_to_string(&subst), "${v:-w}");
    }

    #[test]
    fn test_m4_argument_to_string_literal() {
        let pool: DummyPool = DummyPool::new(vec![]);
        let arg = M4Argument::Literal("lit".to_string());
        assert_eq!(pool.m4_argument_to_string(&arg, 0), "[lit]");
    }

    #[test]
    fn test_node_to_string_brace_and_subshell() {
        let echo = Word::Single(WordFragment::Literal("echo".to_string()));
        let hi = Word::Single(WordFragment::Literal("hi".to_string()));
        let cmd = Node::new(None, None, NodeKind::Cmd(vec![echo.clone(), hi.clone()]));
        let brace = Node::new(None, None, NodeKind::Brace(vec![0]));
        let subshell = Node::new(None, None, NodeKind::Subshell(vec![0]));
        let pool = DummyPool::new(vec![cmd.clone(), brace.clone(), subshell.clone()]);
        assert_eq!(pool.node_to_string(1, 0), "{\n  echo hi\n}");
        assert_eq!(pool.node_to_string(2, 0), "(\n  echo hi\n)");
    }

    #[test]
    fn test_node_to_string_while_until() {
        let w = Word::Single(WordFragment::Literal("f".to_string()));
        let cond = Condition::Cond(Operator::Empty(w.clone()));
        let body = Node::new(None, None, NodeKind::Cmd(vec![w.clone()]));
        let gbp = GuardBodyPair {
            condition: cond.clone(),
            body: vec![0],
        };
        let while_node = Node::new(None, None, NodeKind::While(gbp.clone()));
        let until_node = Node::new(None, None, NodeKind::Until(gbp));
        let pool = DummyPool::new(vec![body.clone(), while_node.clone(), until_node.clone()]);
        assert_eq!(pool.node_to_string(1, 0), "while test -z f; do\n  f\ndone");
        assert_eq!(pool.node_to_string(2, 0), "until test -z f; do\n  f\ndone");
    }

    #[test]
    fn test_node_to_string_if_else() {
        let w = Word::Single(WordFragment::Literal("t".to_string()));
        let cond = Condition::Cond(Operator::Empty(w.clone()));
        let cmd_true = Node::new(None, None, NodeKind::Cmd(vec![w.clone()]));
        let cmd_false = Node::new(
            None,
            None,
            NodeKind::Cmd(vec![Word::Single(WordFragment::Literal("f".to_string()))]),
        );
        let cmd_else = Node::new(
            None,
            None,
            NodeKind::Cmd(vec![Word::Single(WordFragment::Literal("e".to_string()))]),
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
            NodeKind::If {
                conditionals: vec![gbp1.clone()],
                else_branch: vec![],
            },
        );
        let if_else_node = Node::new(
            None,
            None,
            NodeKind::If {
                conditionals: vec![gbp1, gbp2],
                else_branch: vec![2],
            },
        );
        let pool = DummyPool::new(vec![
            cmd_true.clone(),
            cmd_false.clone(),
            cmd_else.clone(),
            if_node.clone(),
            if_else_node.clone(),
        ]);
        assert_eq!(pool.node_to_string(3, 0), "if test -z t; then\n  t\nfi");
        assert_eq!(
            pool.node_to_string(4, 0),
            "if test -z t; then\n  t\nelse if test -z t; then\n  f\nelse\n  e\nfi"
        );
    }

    #[test]
    fn test_node_to_string_for() {
        let cmd = Node::new(
            None,
            None,
            NodeKind::Cmd(vec![Word::Single(WordFragment::Literal("x".to_string()))]),
        );
        let words = vec![
            Word::Single(WordFragment::Literal("1".to_string())),
            Word::Single(WordFragment::Literal("2".to_string())),
        ];
        let for_node = Node::new(
            None,
            None,
            NodeKind::For {
                var: "i".to_string(),
                words: words.clone(),
                body: vec![0],
            },
        );
        let pool = DummyPool::new(vec![cmd.clone(), for_node.clone()]);
        assert_eq!(
            pool.node_to_string(1, 0),
            "for i in \"1\" \"2\"; do\n  x\ndone"
        );
    }

    #[test]
    fn test_node_to_string_case() {
        let cmd = Node::new(
            None,
            None,
            NodeKind::Cmd(vec![Word::Single(WordFragment::Literal("c".to_string()))]),
        );
        let pat1 = Word::Single(WordFragment::Literal("a".to_string()));
        let pat2 = Word::Single(WordFragment::Literal("b".to_string()));
        let arm = PatternBodyPair {
            patterns: vec![pat1.clone(), pat2.clone()],
            body: vec![0],
        };
        let case_node = Node::new(
            None,
            None,
            NodeKind::Case {
                word: Word::Single(WordFragment::Literal("x".to_string())),
                arms: vec![arm],
            },
        );
        let pool = DummyPool::new(vec![cmd.clone(), case_node.clone()]);
        assert_eq!(
            pool.node_to_string(1, 0),
            "case x in\n  a|b)\n    c\n    ;;\nesac"
        );
    }

    #[test]
    fn test_node_to_string_and_or_pipe() {
        let cmd = Node::new(
            None,
            None,
            NodeKind::Cmd(vec![Word::Single(WordFragment::Literal("c".to_string()))]),
        );
        let eq = Operator::Eq(
            Word::Single(WordFragment::Literal("a".to_string())),
            Word::Single(WordFragment::Literal("b".to_string())),
        );
        let cond = Condition::Cond(eq.clone());
        let and_node = Node::new(None, None, NodeKind::And(cond.clone(), 0));
        let or_node = Node::new(None, None, NodeKind::Or(cond.clone(), 0));
        let pipe_node = Node::new(None, None, NodeKind::Pipe(false, vec![0, 0]));
        let bang_pipe = Node::new(None, None, NodeKind::Pipe(true, vec![0, 0]));
        let pool = DummyPool::new(vec![
            cmd.clone(),
            and_node.clone(),
            or_node.clone(),
            pipe_node.clone(),
            bang_pipe.clone(),
        ]);
        assert_eq!(pool.node_to_string(1, 0), "test a = b && c");
        assert_eq!(pool.node_to_string(2, 0), "test a = b || c");
        assert_eq!(pool.node_to_string(3, 0), "c | c");
        assert_eq!(pool.node_to_string(4, 0), "!c | c");
    }

    #[test]
    fn test_node_to_string_redirect_and_background() {
        let cmd = Node::new(
            None,
            None,
            NodeKind::Cmd(vec![Word::Single(WordFragment::Literal("cmd".to_string()))]),
        );
        let r1 = Redirect::Read(None, Word::Single(WordFragment::Literal("in".to_string())));
        let r2 = Redirect::Write(
            Some(1),
            Word::Single(WordFragment::Literal("out".to_string())),
        );
        let rd_node = Node::new(None, None, NodeKind::Redirect(0, vec![r1, r2]));
        let bg_node = Node::new(None, None, NodeKind::Background(0));
        let pool = DummyPool::new(vec![cmd.clone(), rd_node.clone(), bg_node.clone()]);
        assert_eq!(pool.node_to_string(1, 0), "cmd < in 1> out");
        assert_eq!(pool.node_to_string(2, 0), "cmd &");
    }

    #[test]
    fn test_node_to_string_functiondef_and_macro() {
        let body = Node::new(
            None,
            None,
            NodeKind::Cmd(vec![Word::Single(WordFragment::Literal("b".to_string()))]),
        );
        let func = Node::new(
            None,
            None,
            NodeKind::FunctionDef {
                name: "f".to_string(),
                body: 0,
            },
        );
        let m4 = M4Macro::new(
            "m".to_string(),
            vec![
                M4Argument::Literal("a".to_string()),
                M4Argument::Word(Word::Single(WordFragment::Literal("w".to_string()))),
            ],
        );
        let mac = Node::new(None, None, NodeKind::Macro(m4.clone()));
        let pool = DummyPool::new(vec![body.clone(), func.clone(), mac.clone()]);
        assert_eq!(pool.node_to_string(1, 0), "function f () b");
        assert_eq!(pool.node_to_string(2, 0), "m([a],\n  w)");
    }
}
