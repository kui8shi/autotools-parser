//! AST builder for node representations
use std::fmt::Debug;
use std::marker::PhantomData;

use crate::ast::am::{
    AmAssignOp, AmAssignment, AmConditional, AmLine, AmRule, AmVar, AmWord, AmWordFragment, MayAm,
};
use crate::ast::builder::QuoteWordKind;
use crate::ast::minimal::{Word, WordFragment};
use crate::ast::node::{AcCommand, AcWord, AcWordFragment, Node, NodeId, ShellCommand};
use crate::ast::sh::{ShWord, ShWordFragment};
use crate::ast::{map_arith, map_param, MayM4, ParameterSubstitution};
use crate::m4_macro::{M4Macro, SideEffect};
use crate::{
    ast::{
        condition::Condition, minimal::GuardBodyPair, AndOr, PatternBodyPair, Redirect,
        RedirectOrCmdWord, RedirectOrEnvVar,
    },
    m4_macro::M4Argument,
};

use super::minimal_builder::{compress, ConditionBuilder, M4Simplifier};
use super::{
    BuilderBase, BuilderError, CaseFragments, CommandGroup, ConcatWordKind, ForFragments,
    GuardBodyPairGroup, IfFragments, LoopKind, M4Builder, MakeBuilder, Newline, RedirectKind,
    SeparatorKind, ShellBuilder, WordKind,
};
use slab::Slab;
use ShellCommand::*;

/// A type alias for NodeBuilder specialized for autoconf parsing.
pub type AutoconfNodeBuilder<U> = NodeBuilder<AcCommand, AcWord, AcWordFragment, U>;
/// A type alias for NodeBuilder specialized for automake parsing.
pub type AutomakeNodeBuilder<U> = NodeBuilder<AmLine, AmWord, AmWordFragment, U>;
/// A type alias for NodeBuilder specialized for shell parsing.
pub type ShellNodeBuilder<U> = NodeBuilder<ShellCommand<ShWord>, ShWord, ShWordFragment, U>;

/// TODO: add doc comments
#[derive(Debug, Clone)]
pub struct NodeBuilder<C, W, F, U> {
    /// all nodes are put here
    pub nodes: Slab<Node<C, U>>,
    _word: PhantomData<W>,
    _fragment: PhantomData<F>,
}

impl<C, W, F, U> Default for NodeBuilder<C, W, F, U> {
    fn default() -> Self {
        Self {
            nodes: Slab::default(),
            _word: PhantomData,
            _fragment: PhantomData,
        }
    }
}

impl<C, W, F, U: Default> NodeBuilder<C, W, F, U> {
    fn new_node(&mut self, cmd: C) -> NodeId {
        self.new_node_with_comment(cmd, None)
    }

    fn new_node_with_comment(&mut self, cmd: C, comments: Option<String>) -> NodeId {
        self.nodes
            .insert(Node::new(comments, None, cmd, Default::default()))
    }

    fn node_mut(&mut self, id: NodeId) -> &mut Node<C, U> {
        &mut self.nodes[id]
    }

    fn node_pop(&mut self, id: NodeId) -> Node<C, U> {
        self.nodes.remove(id)
    }

    fn node_push(&mut self, node: Node<C, U>) -> usize {
        self.nodes.insert(node)
    }
}

impl<C, W, F, U> BuilderBase for NodeBuilder<C, W, F, U> {
    type Command = NodeId;
    type CompoundCommand = NodeId;
    type Word = W;
    type WordFragment = F;
    type Redirect = Redirect<Self::Word>;
    type Error = BuilderError;
}

impl<U> M4Builder for NodeBuilder<AcCommand, AcWord, AcWordFragment, U>
where
    U: Default + Debug,
{
    type M4Macro = M4Macro<Self::Command, Self::Word>;
    type M4Argument = M4Argument<Self::Command, Self::Word>;

    fn macro_into_compound_command(
        &mut self,
        macro_call: M4Macro<Self::Command, Self::Word>,
        redirects: Vec<Self::Redirect>,
    ) -> Result<Self::CompoundCommand, Self::Error> {
        let mut cmd = if let Some(cmd) = self.simplify_m4_macro(&macro_call) {
            cmd
        } else {
            self.new_node(macro_call.into())
        };
        if !redirects.is_empty() {
            cmd = self.new_node(Redirect(cmd, redirects).into())
        }
        Ok(cmd)
    }

    fn macro_into_word(
        &mut self,
        macro_call: Self::M4Macro,
    ) -> Result<Self::WordFragment, Self::Error> {
        Ok(MayM4::Macro(macro_call).into())
    }

    fn macro_call(
        &mut self,
        name: String,
        args: Vec<M4Argument<Self::Command, Self::Word>>,
        effects: Option<SideEffect>,
        original_name: Option<String>,
    ) -> Result<Self::M4Macro, Self::Error> {
        Ok(M4Macro::new_with_side_effect(
            name,
            args,
            effects,
            original_name,
        ))
    }
}

impl<C, W, F, U> NodeBuilder<C, W, F, U>
where
    C: From<ShellCommand<W>> + Into<Option<ShellCommand<W>>> + Clone,
    F: From<WordFragment<String, NodeId, W>>
        + Into<Option<WordFragment<String, NodeId, W>>>
        + Clone
        + Debug,
    U: Default + Debug,
{
    fn map_quote_word(kind: QuoteWordKind<F>) -> Result<Option<F>, <Self as BuilderBase>::Error> {
        use super::QuoteWordKind::*;
        let word = match kind {
            Simple(f) => {
                if let Some(WordFragment::Literal(s)) = f.clone().into() {
                    if s.is_empty() {
                        None // empty string
                    } else {
                        Some(f)
                    }
                } else {
                    Some(f)
                }
            }
            SingleQuoted(s) if s.is_empty() => {
                None // empty string
            }
            SingleQuoted(s) => Some(WordFragment::Literal(s.into()).into()),
            DoubleQuoted(mut v) => match v.len() {
                0 => None,                   // empty string
                1 => Some(v.pop().unwrap()), // like "foo", "${foo}"
                _ => Some(
                    WordFragment::DoubleQuoted(v.into_iter().filter_map(|m| m.into()).collect())
                        .into(),
                ),
            },
        };
        Ok(word)
    }
}

impl<U: Default> MakeBuilder for NodeBuilder<AmLine, AmWord, AmWordFragment, U> {
    type Statement = NodeId;

    fn rule(
        &mut self,
        target: Vec<Self::Word>,
        dependency: Vec<Self::Word>,
        recipe: Vec<Self::Statement>,
    ) -> Result<Self::Statement, Self::Error> {
        Ok(self.new_node(AmLine::Rule(AmRule {
            target,
            dependency,
            recipe,
        })))
    }

    fn conditional(
        &mut self,
        negated: bool,
        guard_var: String,
        then: Vec<Self::Statement>,
        otherwise: Vec<Self::Statement>,
    ) -> Result<Self::Statement, Self::Error> {
        Ok(self.new_node(AmLine::Conditional(AmConditional {
            negated,
            guard_var,
            then,
            otherwise,
        })))
    }

    fn assignment(
        &mut self,
        lhs: Self::Word,
        op: AmAssignOp,
        rhs: Vec<Self::Word>,
    ) -> Result<Self::Statement, Self::Error> {
        Ok(self.new_node(AmLine::Assignment(AmAssignment { lhs, op, rhs })))
    }

    fn shell_as_recipe(&mut self, cmd: Self::Command) -> Result<Self::Statement, Self::Error> {
        Ok(cmd)
    }

    fn include(&mut self, path: Self::Word) -> Result<Self::Statement, Self::Error> {
        Ok(self.new_node(AmLine::Include(path)))
    }

    fn variable(&mut self, var: AmVar) -> Result<Self::WordFragment, Self::Error> {
        Ok(MayAm::Automake(var))
    }
}

impl<C, W, F, U> ShellBuilder for NodeBuilder<C, W, F, U>
where
    C: From<ShellCommand<Self::Word>> + Into<Option<ShellCommand<Self::Word>>> + Clone + Debug,
    W: From<Word<F>> + Into<Word<F>> + Into<Option<String>> + From<String> + Clone + Debug,
    F: From<WordFragment<String, NodeId, W>>
        + Into<Option<WordFragment<String, NodeId, W>>>
        + Into<Option<String>>
        + Clone
        + Debug,
    U: Default + Debug,
{
    type CommandList = Self::Command;
    type ListableCommand = Self::Command;
    type PipeableCommand = Self::Command;

    /// Constructs a `Command::Job` node with the provided inputs if the command
    /// was delimited by an ampersand or the command itself otherwise.
    fn complete_command(
        &mut self,
        pre_cmd_comments: Vec<Newline>,
        list: Self::CommandList,
        _separator: SeparatorKind,
        _cmd_comment: Option<Newline>,
        range: (usize, usize),
    ) -> Result<Self::Command, Self::Error> {
        let raw_cmd = self.node_mut(list);
        let comments = (!pre_cmd_comments.is_empty())
            .then_some(
                pre_cmd_comments
                    .into_iter()
                    .filter_map(|n| n.0)
                    .collect::<Vec<_>>()
                    .join("\n"),
            )
            .filter(|c: &String| !c.is_empty());
        let complete_cmd = {
            raw_cmd.comment = comments;
            raw_cmd.range = vec![range];
            list
        };
        Ok(complete_cmd)
    }

    /// Constructs a `Command::List` node with the provided inputs.
    fn and_or_list(
        &mut self,
        first: Self::ListableCommand,
        rest: Vec<(Vec<Newline>, AndOr<Self::ListableCommand>)>,
    ) -> Result<Self::CommandList, Self::Error> {
        if rest.is_empty() {
            Ok(first)
        } else {
            let mut cmd = first;
            for (comments, andor) in rest {
                let comments = (!comments.is_empty())
                    .then_some(comments.into_iter().filter_map(|n| n.0).collect());
                match andor {
                    AndOr::And(next) => {
                        let cond = self.make_condition(cmd)?;
                        cmd = self.new_node_with_comment(And(cond, next.into()).into(), comments)
                    }
                    AndOr::Or(next) => {
                        let cond = self.make_condition(cmd)?;
                        cmd = self.new_node_with_comment(Or(cond, next.into()).into(), comments)
                    }
                }
            }
            Ok(cmd)
        }
    }

    /// Constructs a `Command::Pipe` node with the provided inputs or a `Command::Simple`
    /// node if only a single command with no status inversion is supplied.
    fn pipeline(
        &mut self,
        bang: bool,
        mut cmds: Vec<(Vec<Newline>, Self::PipeableCommand)>,
    ) -> Result<Self::ListableCommand, Self::Error> {
        debug_assert!(!cmds.is_empty());
        if cmds.len() > 1 {
            Ok(self.new_node(Pipe(bang, cmds.into_iter().map(|(_, c)| c).collect()).into()))
        } else if bang {
            // FIXME Suppport bang in the case of single command with other than fake Pipe.
            Ok(self.new_node(Pipe(bang, vec![cmds.pop().unwrap().1]).into()))
        } else {
            Ok(cmds.pop().unwrap().1)
        }
    }

    /// Constructs a `Command::Simple` node with the provided inputs.
    fn simple_command(
        &mut self,
        redirects_or_env_vars: Vec<RedirectOrEnvVar<Self::Redirect, String, Self::Word>>,
        redirects_or_cmd_words: Vec<RedirectOrCmdWord<Self::Redirect, Self::Word>>,
    ) -> Result<Self::PipeableCommand, Self::Error> {
        let mut assignments = redirects_or_env_vars
            .into_iter()
            .map(|roev| match roev {
                RedirectOrEnvVar::Redirect(_) => {
                    // @kui8shi
                    // for simplicity of AST, we confine the command syntax
                    // to disallow redirections before any command word appears
                    Err(BuilderError::UnsupportedSyntax)
                }
                RedirectOrEnvVar::EnvVar(k, v) => {
                    Ok(self.new_node(Assignment(k.into(), v.unwrap_or(Word::Empty.into())).into()))
                }
            })
            .collect::<Result<Vec<Self::Command>, _>>()?;

        let mut redirects = Vec::new();
        let mut cmd_words = Vec::new();
        for r_or_c in redirects_or_cmd_words {
            match r_or_c {
                RedirectOrCmdWord::Redirect(r) => redirects.push(r),
                RedirectOrCmdWord::CmdWord(w) => cmd_words.push(w),
            }
        }
        if cmd_words.is_empty() {
            assert!(!assignments.is_empty());
            if assignments.len() == 1 {
                Ok(assignments.pop().unwrap())
            } else {
                Ok(self.new_node(Brace(assignments).into()))
            }
        } else {
            let mut cmd = self.new_node(Cmd(cmd_words).into());
            if !redirects.is_empty() {
                cmd = self.new_node(Redirect(cmd, redirects).into());
            }
            if assignments.is_empty() {
                Ok(cmd)
            } else {
                assignments.push(cmd);
                Ok(self.new_node(Subshell(assignments).into()))
            }
        }
    }

    /// Constructs a `CompoundCommand::Brace` node with the provided inputs.
    fn brace_group(
        &mut self,
        cmd_group: CommandGroup<Self::Command>,
        mut redirects: Vec<Self::Redirect>,
    ) -> Result<Self::CompoundCommand, Self::Error> {
        let mut cmds = cmd_group.commands;
        cmds.shrink_to_fit();
        redirects.shrink_to_fit();
        let mut cmd = self.new_node(Brace(cmds).into());
        if !redirects.is_empty() {
            cmd = self.new_node(Redirect(cmd, redirects).into())
        }
        Ok(cmd)
    }

    /// Constructs a `CompoundCommand::Subshell` node with the provided inputs.
    fn subshell(
        &mut self,
        cmd_group: CommandGroup<Self::Command>,
        mut redirects: Vec<Self::Redirect>,
    ) -> Result<Self::CompoundCommand, Self::Error> {
        let mut cmds = cmd_group.commands;
        cmds.shrink_to_fit();
        redirects.shrink_to_fit();
        let mut cmd = self.new_node(Subshell(cmds).into());
        if !redirects.is_empty() {
            cmd = self.new_node(Redirect(cmd, redirects).into())
        }
        Ok(cmd)
    }

    /// Constructs a `CompoundCommand::Loop` node with the provided inputs.
    fn loop_command(
        &mut self,
        kind: LoopKind,
        guard_body_pair: GuardBodyPairGroup<Self::Command>,
        mut redirects: Vec<Self::Redirect>,
    ) -> Result<Self::CompoundCommand, Self::Error> {
        let mut guard = guard_body_pair.guard.commands;
        let mut body = guard_body_pair.body.commands;

        guard.shrink_to_fit();
        body.shrink_to_fit();
        redirects.shrink_to_fit();

        let guard_body_pair = GuardBodyPair {
            condition: self.make_condition(guard.pop().unwrap())?,
            body,
        };

        let mut cmd = match kind {
            LoopKind::While => self.new_node(While(guard_body_pair).into()),
            LoopKind::Until => self.new_node(Until(guard_body_pair).into()),
        };
        if !redirects.is_empty() {
            cmd = self.new_node(Redirect(cmd, redirects).into())
        }
        Ok(cmd)
    }

    /// Constructs a `CompoundCommand::If` node with the provided inputs.
    fn if_command(
        &mut self,
        fragments: IfFragments<Self::Command>,
        mut redirects: Vec<Self::Redirect>,
    ) -> Result<Self::CompoundCommand, Self::Error> {
        let IfFragments {
            conditionals,
            else_branch,
        } = fragments;

        let conditionals = conditionals
            .into_iter()
            .map(|pair| {
                let mut guard = pair.guard.commands;
                let mut body = pair.body.commands;

                guard.shrink_to_fit();
                body.shrink_to_fit();

                Ok(GuardBodyPair {
                    condition: self.make_condition(guard.pop().unwrap())?,
                    body,
                })
            })
            .collect::<Result<Vec<_>, _>>()?;

        let else_branch = else_branch
            .map(
                |CommandGroup {
                     commands: mut els, ..
                 }| {
                    els.shrink_to_fit();
                    els
                },
            )
            .unwrap_or_default();

        redirects.shrink_to_fit();

        let mut cmd = self.new_node(
            If {
                conditionals,
                else_branch,
            }
            .into(),
        );
        if !redirects.is_empty() {
            cmd = self.new_node(Redirect(cmd, redirects).into())
        }
        Ok(cmd)
    }

    /// Constructs a `CompoundCommand::For` node with the provided inputs.
    fn for_command(
        &mut self,
        fragments: ForFragments<Self::Command, Self::Word>,
        mut redirects: Vec<Self::Redirect>,
    ) -> Result<Self::CompoundCommand, Self::Error> {
        let words = fragments
            .words
            .map(|(_, mut words, _)| {
                words.shrink_to_fit();
                words
            })
            .unwrap_or_default();

        let mut body = fragments.body.commands;
        body.shrink_to_fit();
        redirects.shrink_to_fit();

        let mut cmd = self.new_node(
            For {
                var: fragments.var,
                words,
                body,
            }
            .into(),
        );
        if !redirects.is_empty() {
            cmd = self.new_node(Redirect(cmd, redirects).into())
        }
        Ok(cmd)
    }

    /// Constructs a `CompoundCommand::Case` node with the provided inputs.
    fn case_command(
        &mut self,
        fragments: CaseFragments<Self::Command, Self::Word>,
        mut redirects: Vec<Self::Redirect>,
    ) -> Result<Self::CompoundCommand, Self::Error> {
        let arms = fragments
            .arms
            .into_iter()
            .map(|arm| {
                let comments = arm
                    .patterns
                    .pre_pattern_comments
                    .into_iter()
                    .flat_map(|newline| newline.0)
                    .collect();

                let mut patterns = arm.patterns.pattern_alternatives;
                patterns.shrink_to_fit();

                let mut body = arm.body.commands;
                body.shrink_to_fit();

                PatternBodyPair {
                    comments,
                    patterns,
                    body,
                }
            })
            .collect();

        redirects.shrink_to_fit();

        let mut cmd = self.new_node(
            Case {
                word: fragments.word,
                arms,
            }
            .into(),
        );
        if !redirects.is_empty() {
            cmd = self.new_node(Redirect(cmd, redirects).into())
        }
        Ok(cmd)
    }

    /// Converts a `CompoundCommand` into a `PipeableCommand`.
    fn compound_command_into_pipeable(
        &mut self,
        cmd: Self::CompoundCommand,
    ) -> Result<Self::PipeableCommand, Self::Error> {
        Ok(cmd)
    }

    /// Constructs a `Command::FunctionDef` node with the provided inputs.
    fn function_declaration(
        &mut self,
        name: String,
        _post_name_comments: Vec<Newline>,
        body: Self::CompoundCommand,
    ) -> Result<Self::PipeableCommand, Self::Error> {
        Ok(self.new_node(FunctionDef { name, body }.into()))
    }

    /// Ignored by the builder.
    fn comments(&mut self, _comments: Vec<Newline>) -> Result<(), Self::Error> {
        Ok(())
    }

    fn word_fragment(
        &mut self,
        kind: WordKind<Self::Command, Self::WordFragment>,
    ) -> Result<Self::WordFragment, Self::Error> {
        macro_rules! map {
            ($pat:expr) => {
                match $pat {
                    Some(w) => Some(self.word(w)?),
                    None => None,
                }
            };
        }
        use super::ParameterSubstitutionKind::*;
        use super::WordKind::*;

        let simple = match kind {
            Literal(s) => WordFragment::Literal(s.into()),
            Escaped(s) => WordFragment::Escaped(s.into()),
            Param(p) => WordFragment::Param(map_param(p)),
            Star => WordFragment::Star,
            Question => WordFragment::Question,
            SquareOpen => WordFragment::SquareOpen,
            SquareClose => WordFragment::SquareClose,
            Tilde => WordFragment::Tilde,
            Colon => WordFragment::Colon,

            CommandSubst(c) => {
                WordFragment::Subst(Box::new(ParameterSubstitution::Command(c.commands)))
            }

            Subst(s) => {
                // Force a move out of the boxed substitution. For some reason doing
                // the deref in the match statment gives a strange borrow failure
                let s = *s;
                let subst = match s {
                    Len(p) => ParameterSubstitution::Len(map_param(p)),
                    Command(c) => ParameterSubstitution::Command(c.commands),
                    Arith(a) => ParameterSubstitution::Arith(a.map(map_arith)),
                    Default(c, p, w) => ParameterSubstitution::Default(c, map_param(p), map!(w)),
                    Assign(c, p, w) => ParameterSubstitution::Assign(c, map_param(p), map!(w)),
                    Error(c, p, w) => ParameterSubstitution::Error(c, map_param(p), map!(w)),
                    Alternative(c, p, w) => {
                        ParameterSubstitution::Alternative(c, map_param(p), map!(w))
                    }
                    RemoveSmallestSuffix(p, w) => {
                        ParameterSubstitution::RemoveSmallestSuffix(map_param(p), map!(w))
                    }
                    RemoveLargestSuffix(p, w) => {
                        ParameterSubstitution::RemoveLargestSuffix(map_param(p), map!(w))
                    }
                    RemoveSmallestPrefix(p, w) => {
                        ParameterSubstitution::RemoveSmallestPrefix(map_param(p), map!(w))
                    }
                    RemoveLargestPrefix(p, w) => {
                        ParameterSubstitution::RemoveLargestPrefix(map_param(p), map!(w))
                    }
                };
                WordFragment::Subst(Box::new(subst))
            }
        };
        Ok(simple.into())
    }

    /// Constructs a `ast::Word` from the provided input.
    fn word(
        &mut self,
        kind: ConcatWordKind<Self::WordFragment>,
    ) -> Result<Self::Word, Self::Error> {
        let word = match compress(kind) {
            ConcatWordKind::Single(s) => {
                Self::map_quote_word(s)?.map_or(Word::Empty, |w| Word::Single(w))
            }
            ConcatWordKind::Concat(words) => Word::Concat(
                words
                    .into_iter()
                    .map(Self::map_quote_word)
                    .collect::<Result<Vec<_>, _>>()?
                    .into_iter()
                    .flatten()
                    .collect(),
            ),
        };

        Ok(word.into())
    }

    /// Constructs a `ast::Redirect` from the provided input.
    fn redirect(&mut self, kind: RedirectKind<Self::Word>) -> Result<Self::Redirect, Self::Error> {
        let io = match kind {
            RedirectKind::Read(fd, path) => Redirect::Read(fd, path),
            RedirectKind::Write(fd, path) => Redirect::Write(fd, path),
            RedirectKind::ReadWrite(fd, path) => Redirect::ReadWrite(fd, path),
            RedirectKind::Append(fd, path) => Redirect::Append(fd, path),
            RedirectKind::Clobber(fd, path) => Redirect::Clobber(fd, path),
            RedirectKind::Heredoc(fd, body) => Redirect::Heredoc(fd, body),
            RedirectKind::DupRead(src, dst) => Redirect::DupRead(src, dst),
            RedirectKind::DupWrite(src, dst) => Redirect::DupWrite(src, dst),
        };

        Ok(io)
    }
}

impl<C, W, F, U> ConditionBuilder for NodeBuilder<C, W, F, U>
where
    C: From<ShellCommand<W>> + Into<Option<ShellCommand<W>>> + Clone,
    W: From<Word<F>> + Into<Word<F>> + Into<Option<String>> + From<String> + Clone + Debug,
    F: From<WordFragment<String, NodeId, W>>
        + Into<Option<WordFragment<String, NodeId, W>>>
        + Into<Option<String>>
        + Clone,
    U: Default + Debug,
{
    fn make_condition(&mut self, cmd: NodeId) -> Result<Condition<NodeId, W>, BuilderError> {
        let node = self.node_pop(cmd);
        if let Some(condition) = match node.cmd.clone().into() {
            Some(Cmd(words)) => {
                let first_word = words.first().unwrap();
                if let Word::Single(f) = first_word.clone().into() {
                    match f.into() {
                        Some(WordFragment::Literal(v)) if v.clone() == "test" => {
                            Some(self.parse_test_command(&words[1..])?)
                        }
                        Some(WordFragment::Literal(v)) if v.clone() == "eval" => {
                            Some(Condition::Eval(Box::new(
                                self.new_node(Cmd(words[1..].to_owned()).into()),
                            )))
                        }
                        Some(WordFragment::Subst(s)) => match s.as_ref() {
                            ParameterSubstitution::Command(cmds) => {
                                Some(Condition::Eval(Box::new(cmds.first().unwrap().to_owned())))
                            }
                            _ => None,
                        },
                        _ => None,
                    }
                } else {
                    None
                }
            }
            Some(And(first, second)) => Some(Condition::And(
                Box::new(first.clone()),
                Box::new(self.make_condition(second.clone())?),
            )),
            Some(Or(first, second)) => Some(Condition::Or(
                Box::new(first.clone()),
                Box::new(self.make_condition(second.clone())?),
            )),
            Some(Pipe(true, cmds)) if cmds.len() == 1 => {
                let cmd = cmds.first().unwrap().clone();
                self.make_condition(cmd).ok().map(|cond| cond.flip())
            }
            _ => None,
        } {
            Ok(condition)
        } else {
            self.node_push(node);
            Ok(Condition::ReturnZero(Box::new(cmd)))
        }
    }
}

impl<U> M4Simplifier for NodeBuilder<AcCommand, AcWord, AcWordFragment, U> where U: Default + Debug {}
