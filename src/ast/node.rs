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
    L: fmt::Display,
{
    /// Retrieve a reference to the AST `Node` identified by `node_id`.
    fn get_node(&self, node_id: NodeId) -> &Node<L>;

    /// Format the AST node with the given `node_id` into a string, indenting each line
    /// by `indent` spaces to represent nested structures.
    fn node_to_string(&self, node_id: NodeId, indent: usize) -> String {
        use self::NodeKind::*;
        let tab = " ".repeat(indent);

        let body_to_string = |cmds: &[NodeId], n| {
            cmds.iter()
                .map(|c| self.node_to_string(*c, n))
                .collect::<Vec<String>>()
                .join("\n")
        };

        match &self.get_node(node_id).kind {
            Assignment(name, word) => format!("{tab}{}={}", name, self.word_to_string(word)),
            Cmd(words) => format!(
                "{tab}{}",
                words
                    .iter()
                    .map(|w| self.word_to_string(w))
                    .collect::<Vec<String>>()
                    .join(" ")
            ),
            Brace(cmds) => format!("{tab}{{\n{}\n{tab}}}", body_to_string(cmds, indent + 2)),
            Subshell(cmds) => format!("{tab}(\n{}\n{tab})", body_to_string(cmds, indent + 2)),
            While(pair) => format!(
                "{tab}while {}; do\n{}\n{tab}done",
                self.condition_to_string(&pair.condition),
                body_to_string(&pair.body, indent + 2)
            ),
            Until(pair) => format!(
                "{tab}until {}; do\n{}\n{tab}done",
                self.condition_to_string(&pair.condition),
                body_to_string(&pair.body, indent + 2)
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
                        body_to_string(&pair.body, indent + 2)
                    )
                };
                let else_if = conditionals
                    .iter()
                    .skip(1)
                    .map(|pair| {
                        format!(
                            "{tab}else if {}; then\n{}\n",
                            self.condition_to_string(&pair.condition),
                            body_to_string(&pair.body, indent + 2)
                        )
                    })
                    .collect::<Vec<String>>()
                    .join("");
                let rest = if else_branch.is_empty() {
                    format!("{tab}fi")
                } else {
                    format!(
                        "{tab}else\n{}\n{tab}fi",
                        body_to_string(else_branch, indent + 2)
                    )
                };
                format!("{}{}{}", first_if, else_if, rest)
            }
            For { var, words, body } => format!(
                "{tab}for {var} in {}; do\n{}\n{tab}done",
                words
                    .iter()
                    .map(|w| self.word_to_string(w))
                    .collect::<Vec<String>>()
                    .join(" "),
                body_to_string(body, indent + 2)
            ),
            Case { word, arms } => format!(
                "{tab}case {} in\n{}\n{tab}esac",
                self.word_to_string(word),
                arms.iter()
                    .map(|arm| format!(
                        "{tab}  {})\n{}\n{tab}    ;;",
                        arm.patterns
                            .iter()
                            .map(|w| self.word_to_string(w))
                            .collect::<Vec<String>>()
                            .join("|"),
                        body_to_string(&arm.body, indent + 4)
                    ))
                    .collect::<Vec<String>>()
                    .join("\n")
            ),
            And(cond, cmd) => format!(
                "{} && {}",
                self.condition_to_string(cond),
                self.node_to_string(*cmd, indent)
            ),
            Or(cond, cmd) => format!(
                "{} || {}",
                self.condition_to_string(cond),
                self.node_to_string(*cmd, indent)
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
                self.node_to_string(*cmd, indent),
                redirects
                    .iter()
                    .map(|r| self.redirect_to_string(r))
                    .collect::<Vec<String>>()
                    .join(" ")
            ),
            Background(cmd) => format!("{} &", self.node_to_string(*cmd, indent)),
            FunctionDef { name, body } => {
                format!("function {name} () {}", self.node_to_string(*body, 0))
            }
            Macro(m4_macro) => format!(
                "{tab}{}({})",
                m4_macro.name,
                m4_macro
                    .args
                    .iter()
                    .map(|arg| self.m4_argument_to_string(arg, indent + 2))
                    .collect::<Vec<String>>()
                    .join(",\n{tab}  ")
            ),
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
            ReturnZero(cmd) => format!("{}", cmd),
        }
    }

    /// Format an M4 macro argument (`M4Argument`) into a string, applying `indent`
    /// spaces for multiline argument formatting.
    fn m4_argument_to_string(&self, arg: &M4Argument<L>, indent: usize) -> String {
        use crate::m4_macro::M4Argument::*;
        let newline = format!("\n{}", " ".repeat(indent));
        match arg {
            Literal(lit) => lit.to_string(),
            Word(word) => self.word_to_string(word),
            Array(words) => format!(
                "{}",
                words
                    .iter()
                    .map(|w| self.word_to_string(w))
                    .collect::<Vec<String>>()
                    .join(if words.len() < 10 { " " } else { &newline })
            ),
            Program(prog) => prog.replace("\n", &newline),
            Commands(cmds) => format!(
                "{}",
                cmds.iter()
                    .map(|c| self.node_to_string(*c, indent))
                    .collect::<Vec<String>>()
                    .join(&newline)
            ),
            Unknown(unknown) => unknown.to_string(),
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
                self.word_to_string(file)
            ),
            Write(fd, file) => format!(
                "{}> {}",
                fd.map_or("".to_string(), |f| f.to_string()),
                self.word_to_string(file)
            ),
            ReadWrite(fd, file) => format!(
                "{}<> {}",
                fd.map_or("".to_string(), |f| f.to_string()),
                self.word_to_string(file)
            ),
            Append(fd, file) => format!(
                "{}>> {}",
                fd.map_or("".to_string(), |f| f.to_string()),
                self.word_to_string(file)
            ),
            Clobber(fd, file) => format!(
                "{}>| {}",
                fd.map_or("".to_string(), |f| f.to_string()),
                self.word_to_string(file)
            ),
            Heredoc(fd, file) => format!(
                "{}<<EOF\n{}\nEOF",
                fd.map_or("".to_string(), |f| f.to_string()),
                self.word_to_string(file)
            ),
            DupRead(fd, file) => format!(
                "{}<&{}",
                fd.map_or("".to_string(), |f| f.to_string()),
                self.word_to_string(file)
            ),
            DupWrite(fd, file) => format!(
                "{}>&{}",
                fd.map_or("".to_string(), |f| f.to_string()),
                self.word_to_string(file)
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
    fn word_to_string(&self, word: &Word<L>) -> String {
        use super::minimal::Word::*;
        match word {
            Empty => "".to_string(),
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
            Single(frag) => self.word_fragment_to_string(frag),
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
                    .map(|c| c.to_string())
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
                    .map_or("".to_string(), |w| self.word_to_string(w))
            ),
            Assign(_, param, word) => format!(
                "${{{}:={}}}",
                param_raw(param),
                word.as_ref()
                    .map_or("".to_string(), |w| self.word_to_string(w))
            ),
            Error(_, param, word) => format!(
                "${{{}:?{}}}",
                param_raw(param),
                word.as_ref()
                    .map_or("".to_string(), |w| self.word_to_string(w))
            ),
            Alternative(_, param, word) => format!(
                "${{{}:+{}}}",
                param_raw(param),
                word.as_ref()
                    .map_or("".to_string(), |w| self.word_to_string(w))
            ),
            RemoveSmallestSuffix(param, pattern) => format!(
                "${{{}%{}}}",
                param_raw(param),
                pattern
                    .as_ref()
                    .map_or("".to_string(), |w| self.word_to_string(w))
            ),
            RemoveLargestSuffix(param, pattern) => format!(
                "${{{}%%{}}}",
                param_raw(param),
                pattern
                    .as_ref()
                    .map_or("".to_string(), |w| self.word_to_string(w))
            ),
            RemoveSmallestPrefix(param, pattern) => format!(
                "${{{}#{}}}",
                param_raw(param),
                pattern
                    .as_ref()
                    .map_or("".to_string(), |w| self.word_to_string(w))
            ),
            RemoveLargestPrefix(param, pattern) => format!(
                "${{{}##{}}}",
                param_raw(param),
                pattern
                    .as_ref()
                    .map_or("".to_string(), |w| self.word_to_string(w))
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
                self.word_to_string(lhs),
                self.word_to_string(rhs)
            ),
            Neq(lhs, rhs) => format!(
                "{} != {}",
                self.word_to_string(lhs),
                self.word_to_string(rhs)
            ),
            Ge(lhs, rhs) => format!(
                "{} -ge {}",
                self.word_to_string(lhs),
                self.word_to_string(rhs)
            ),
            Gt(lhs, rhs) => format!(
                "{} -gt {}",
                self.word_to_string(lhs),
                self.word_to_string(rhs)
            ),
            Le(lhs, rhs) => format!(
                "{} -le {}",
                self.word_to_string(lhs),
                self.word_to_string(rhs)
            ),
            Lt(lhs, rhs) => format!(
                "{} -lt {}",
                self.word_to_string(lhs),
                self.word_to_string(rhs)
            ),
            Empty(w) => format!("-z {}", self.word_to_string(w)),
            NonEmpty(w) => format!("-n {}", self.word_to_string(w)),
            Dir(w) => format!("-d {}", self.word_to_string(w)),
            File(w) => format!("-f {}", self.word_to_string(w)),
            NoExists(w) => format!("! -e {}", self.word_to_string(w)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Arithmetic, Parameter};
    use crate::m4_macro::{M4Argument, M4Macro};

    struct DummyPool<L> {
        nodes: Vec<Node<L>>,
    }

    impl<L: fmt::Display> DummyPool<L> {
        fn new(nodes: Vec<Node<L>>) -> Self {
            DummyPool { nodes }
        }
    }

    impl<L: fmt::Display> NodePool<L> for DummyPool<L> {
        fn get_node(&self, node_id: NodeId) -> &Node<L> {
            &self.nodes[node_id]
        }
    }

    #[test]
    fn test_get_node() {
        let node = Node::new(None, None, NodeKind::<String>::Cmd(vec![]));
        let pool = DummyPool::new(vec![node.clone()]);
        assert_eq!(pool.get_node(0).kind, node.kind);
    }

    #[test]
    fn test_node_to_string_assignment_and_cmd() {
        let word = Word::Single(WordFragment::Literal("value".to_string()));
        let assign_node = Node::new(None, None, NodeKind::Assignment("var".into(), word.clone()));
        let echo = Word::Single(WordFragment::Literal("echo".into()));
        let cmd_node = Node::new(None, None, NodeKind::Cmd(vec![echo.clone(), word.clone()]));
        let pool = DummyPool::new(vec![assign_node, cmd_node]);
        assert_eq!(pool.node_to_string(0, 0), "var=value");
        assert_eq!(pool.node_to_string(1, 2), "  echo value");
    }

    #[test]
    fn test_operator_to_string() {
        let pool: DummyPool<String> = DummyPool::new(vec![]);
        let op = Operator::Eq(
            Word::Single(WordFragment::Literal("a".into())),
            Word::Single(WordFragment::Literal("b".into())),
        );
        assert_eq!(pool.operator_to_string(&op), "a = b");
    }

    #[test]
    fn test_word_and_fragment_to_string() {
        let pool: DummyPool<String> = DummyPool::new(vec![]);
        let frag = WordFragment::DoubleQuoted(vec![WordFragment::Literal("hi".into())]);
        assert_eq!(pool.word_fragment_to_string(&frag), "\"hi\"");
        let word = Word::Concat(vec![
            WordFragment::Literal("a".into()),
            WordFragment::Literal("b".into()),
        ]);
        assert_eq!(pool.word_to_string(&word), "ab");
    }

    #[test]
    fn test_condition_to_string() {
        let pool: DummyPool<String> = DummyPool::new(vec![]);
        let cond = Condition::Cond(Operator::Neq(
            Word::Single(WordFragment::Literal("x".into())),
            Word::Single(WordFragment::Literal("y".into())),
        ));
        assert_eq!(pool.condition_to_string(&cond), "test x != y");
    }

    #[test]
    fn test_redirect_to_string() {
        let pool: DummyPool<String> = DummyPool::new(vec![]);
        let redir = Redirect::Write(None, Word::Single(WordFragment::Literal("out".into())));
        assert_eq!(pool.redirect_to_string(&redir), "> out");
    }

    #[test]
    fn test_parameter_substitution_to_string_default() {
        let pool: DummyPool<String> = DummyPool::new(vec![]);
        let subst = ParameterSubstitution::Default(
            false,
            Parameter::Var("v".into()),
            Some(Word::Single(WordFragment::Literal("w".into()))),
        );
        assert_eq!(pool.parameter_substitution_to_string(&subst), "${v:-w}");
    }

    #[test]
    fn test_m4_argument_to_string_literal() {
        let pool: DummyPool<String> = DummyPool::new(vec![]);
        let arg = M4Argument::Literal("lit".into());
        assert_eq!(pool.m4_argument_to_string(&arg, 0), "lit");
    }
}
