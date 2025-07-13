//! AST builder for node representations
use std::fmt::Debug;

use crate::ast::builder::QuoteWordKind;
use crate::ast::minimal::{Word, WordFragment};
use crate::ast::node::{AcCommand, AcWord, AcWordFragment, Node, NodeId, ShellCommand};
use crate::ast::{map_arith, map_param, MayM4, ParameterSubstitution};
use crate::m4_macro::{M4Macro, SideEffect};
use crate::{
    ast::{
        minimal::{Condition, GuardBodyPair},
        AndOr, PatternBodyPair, Redirect, RedirectOrCmdWord, RedirectOrEnvVar,
    },
    m4_macro::M4Argument,
};

use super::minimal_builder::ConditionBuilder;
use super::{
    BuilderBase, BuilderError, CaseFragments, CommandGroup, ConcatWordKind, ForFragments,
    GuardBodyPairGroup, IfFragments, LoopKind, M4Builder, Newline, RedirectKind, SeparatorKind,
    ShellBuilder, WordKind,
};
use slab::Slab;
use ShellCommand::*;
type AcNode<L, U> = Node<AcCommand<L>, U>;
/// TODO: add doc comments
#[derive(Debug, Default, Clone)]
pub struct AcNodeBuilder<L, U> {
    /// all nodes are put here
    pub nodes: Slab<AcNode<L, U>>,
}

impl<L, U: Default> AcNodeBuilder<L, U> {
    fn new_node(&mut self, cmd: AcCommand<L>) -> NodeId {
        self.new_node_with_comment(cmd, None)
    }

    fn new_node_with_comment(&mut self, cmd: AcCommand<L>, comments: Option<String>) -> NodeId {
        self.nodes
            .insert(Node::new(comments, None, cmd, Default::default()))
    }

    fn node_mut(&mut self, id: NodeId) -> &mut AcNode<L, U> {
        &mut self.nodes[id]
    }

    fn node_pop(&mut self, id: NodeId) -> AcNode<L, U> {
        self.nodes.remove(id)
    }

    fn node_push(&mut self, node: AcNode<L, U>) -> usize {
        self.nodes.insert(node)
    }
}

impl<L, U> BuilderBase for AcNodeBuilder<L, U> {
    type Command = NodeId;
    type CompoundCommand = NodeId;
    type Word = AcWord<L>;
    type WordFragment =
        MayM4<WordFragment<L, Self::Command, Self::Word>, M4Macro<Self::Command, Self::Word>>;
    type Redirect = Redirect<Self::Word>;
    type Error = BuilderError;
}

impl<L, U> M4Builder for AcNodeBuilder<L, U>
where
    L: Into<String> + From<String> + Clone + Debug,
    U: Default,
{
    type M4Macro = M4Macro<Self::Command, Self::Word>;
    type M4Argument = M4Argument<Self::Command, Self::Word>;

    fn macro_into_compound_command(
        &mut self,
        macro_call: M4Macro<Self::Command, Self::Word>,
        redirects: Vec<Self::Redirect>,
    ) -> Result<Self::CompoundCommand, Self::Error> {
        let mut cmd = self.new_node(AcCommand::new_macro(macro_call));
        if !redirects.is_empty() {
            cmd = self.new_node(AcCommand::new_cmd(Redirect(cmd, redirects)))
        }
        Ok(cmd)
    }

    fn macro_into_word(
        &mut self,
        macro_call: Self::M4Macro,
    ) -> Result<Self::WordFragment, Self::Error> {
        Ok(MayM4::Macro(macro_call))
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

impl<L, U> ShellBuilder for AcNodeBuilder<L, U>
where
    L: Into<String> + From<String> + Clone + Debug,
    U: Default,
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
            raw_cmd.range = Some(range);
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
                        cmd = self.new_node_with_comment(
                            AcCommand::new_cmd(And(cond, next.into())),
                            comments,
                        )
                    }
                    AndOr::Or(next) => {
                        let cond = self.make_condition(cmd)?;
                        cmd = self.new_node_with_comment(
                            AcCommand::new_cmd(Or(cond, next.into())),
                            comments,
                        )
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
            Ok(self.new_node(AcCommand::new_cmd(Pipe(
                bang,
                cmds.into_iter().map(|(_, c)| c).collect(),
            ))))
        } else if bang {
            // FIXME Suppport bang in the case of single command with other than fake Pipe.
            Ok(self.new_node(AcCommand::new_cmd(Pipe(bang, vec![cmds.pop().unwrap().1]))))
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
                RedirectOrEnvVar::EnvVar(k, v) => Ok(self.new_node(AcCommand::new_cmd(
                    Assignment(k.into(), v.unwrap_or(Word::Empty.into())),
                ))),
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
                Ok(self.new_node(AcCommand::new_cmd(Brace(assignments))))
            }
        } else {
            let mut cmd = self.new_node(AcCommand::new_cmd(Cmd(cmd_words)));
            if !redirects.is_empty() {
                cmd = self.new_node(AcCommand::new_cmd(Redirect(cmd, redirects)));
            }
            if assignments.is_empty() {
                Ok(cmd)
            } else {
                assignments.push(cmd);
                Ok(self.new_node(AcCommand::new_cmd(Subshell(assignments))))
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
        let mut cmd = self.new_node(AcCommand::new_cmd(Brace(cmds)));
        if !redirects.is_empty() {
            cmd = self.new_node(AcCommand::new_cmd(Redirect(cmd, redirects)))
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
        let mut cmd = self.new_node(AcCommand::new_cmd(Subshell(cmds)));
        if !redirects.is_empty() {
            cmd = self.new_node(AcCommand::new_cmd(Redirect(cmd, redirects)))
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
            LoopKind::While => self.new_node(AcCommand::new_cmd(While(guard_body_pair))),
            LoopKind::Until => self.new_node(AcCommand::new_cmd(Until(guard_body_pair))),
        };
        if !redirects.is_empty() {
            cmd = self.new_node(AcCommand::new_cmd(Redirect(cmd, redirects)))
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
            .map(|gbp| {
                let mut guard = gbp.guard.commands;
                let mut body = gbp.body.commands;

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

        let mut cmd = self.new_node(AcCommand::new_cmd(If {
            conditionals,
            else_branch,
        }));
        if !redirects.is_empty() {
            cmd = self.new_node(AcCommand::new_cmd(Redirect(cmd, redirects)))
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

        let mut cmd = self.new_node(AcCommand::new_cmd(For {
            var: fragments.var,
            words,
            body,
        }));
        if !redirects.is_empty() {
            cmd = self.new_node(AcCommand::new_cmd(Redirect(cmd, redirects)))
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
                let mut patterns = arm.patterns.pattern_alternatives;
                patterns.shrink_to_fit();

                let mut body = arm.body.commands;
                body.shrink_to_fit();

                PatternBodyPair { patterns, body }
            })
            .collect();

        redirects.shrink_to_fit();

        let mut cmd = self.new_node(AcCommand::new_cmd(Case {
            word: fragments.word,
            arms,
        }));
        if !redirects.is_empty() {
            cmd = self.new_node(AcCommand::new_cmd(Redirect(cmd, redirects)))
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
        Ok(self.new_node(AcCommand::new_cmd(FunctionDef { name, body })))
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
        Ok(MayM4::Shell(simple))
    }

    /// Constructs a `ast::Word` from the provided input.
    fn word(
        &mut self,
        kind: ConcatWordKind<Self::WordFragment>,
    ) -> Result<Self::Word, Self::Error> {
        let map_quote_word = |kind: QuoteWordKind<MayM4<WordFragment<L, _, _>, _>>| {
            use super::super::MayM4::*;
            use super::QuoteWordKind::*;
            let word = match kind {
                Simple(Shell(WordFragment::Literal(s))) if s.clone().into().is_empty() => {
                    None // empty string
                }
                SingleQuoted(s) if s.is_empty() => {
                    None // empty string
                }
                Simple(s) => Some(s),
                SingleQuoted(s) => Some(Shell(WordFragment::Literal(s.into()))),
                DoubleQuoted(mut v) => match v.len() {
                    0 => None,                   // empty string
                    1 => Some(v.pop().unwrap()), // like "foo", "${foo}"
                    _ => Some(Shell(WordFragment::DoubleQuoted(
                        v.into_iter()
                            .filter_map(|m| match m {
                                // @kui8shi
                                // To simplify the data structure, we ignore double quoted macro
                                // words for now.
                                Macro(_) => None,
                                Shell(w) => Some(w),
                            })
                            .collect(),
                    ))),
                },
            };
            Ok(word)
        };

        let word = match kind {
            ConcatWordKind::Single(s) => {
                map_quote_word(s)?.map_or(Word::Empty, |w| Word::Single(w))
            }
            ConcatWordKind::Concat(words) => Word::Concat(
                words
                    .into_iter()
                    .map(map_quote_word)
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

impl<L, U> ConditionBuilder<NodeId, AcWord<L>, AcWordFragment<L>> for AcNodeBuilder<L, U>
where
    L: Into<String> + Clone + Debug,
    U: Default,
{
    fn make_condition(
        &mut self,
        cmd: NodeId,
    ) -> Result<Condition<<Self as BuilderBase>::Command, <Self as BuilderBase>::Word>, BuilderError>
    {
        let node = self.node_pop(cmd);
        match &node.cmd.0 {
            MayM4::Shell(Cmd(words)) => {
                let first_word = words.first().unwrap();
                if let Word::Single(may_word) = &first_word.0 {
                    if let MayM4::Shell(w) = may_word {
                        match w {
                            WordFragment::Literal(v) if v.clone().into() == "test" => {
                                Some(Condition::Cond(self.parse_condition(&words[1..])?))
                            }
                            WordFragment::Literal(v) if v.clone().into() == "eval" => {
                                Some(Condition::Eval(vec![
                                    self.new_node(AcCommand::new_cmd(Cmd(words[1..].to_owned())))
                                ]))
                            }
                            WordFragment::Subst(s) => match s.as_ref() {
                                ParameterSubstitution::Command(cmds) => {
                                    Some(Condition::Eval(cmds.clone()))
                                }
                                _ => None,
                            },
                            _ => None,
                        }
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            MayM4::Shell(And(first, second)) => Some(Condition::And(
                Box::new(first.clone()),
                Box::new(self.make_condition(second.clone())?),
            )),
            MayM4::Shell(Or(first, second)) => Some(Condition::Or(
                Box::new(first.clone()),
                Box::new(self.make_condition(second.clone())?),
            )),
            _ => None,
        }
        .map_or(
            {
                self.node_push(node);
                Ok(Condition::ReturnZero(Box::new(cmd)))
            },
            Ok,
        )
    }
}
