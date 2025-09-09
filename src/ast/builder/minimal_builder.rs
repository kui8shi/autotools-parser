use std::fmt::Debug;
use std::marker::PhantomData;

use super::{
    BuilderBase, BuilderError, CaseArm, CaseFragments, CasePatternFragments, CommandGroup,
    ConcatWordKind, ForFragments, GuardBodyPairGroup, IfFragments, LoopKind, M4Builder, Newline,
    QuoteWordKind, RedirectKind, SeparatorKind, ShellBuilder, WordKind,
};

use crate::ast::builder::{Coalesce, CoalesceResult};
use crate::ast::minimal::{
    AcWord, Command::*, MinimalCommand, MinimalCompoundCommand, MinimalWordFragment,
};
use crate::ast::{map_arith, map_param, MayM4};
use crate::{
    ast::{
        condition::{Condition, Operator},
        minimal::{AcCommand, CompoundCommand, GuardBodyPair, Word, WordFragment},
        AndOr, ParameterSubstitution, PatternBodyPair, Redirect, RedirectOrCmdWord,
        RedirectOrEnvVar,
    },
    m4_macro::{M4Argument, M4Macro, SideEffect},
};

use MayM4::*;

/// TODO: add doc comments
#[derive(Debug, Default, PartialEq, Eq, Clone)]
pub struct MinimalBuilder<T> {
    phantom_data: PhantomData<T>,
}

impl<L> BuilderBase for MinimalBuilder<L> {
    type CompoundCommand = MinimalCompoundCommand<L>;
    type Command = MinimalCommand<L>;
    type WordFragment = MinimalWordFragment<L>;
    type Word = AcWord<L>;
    type Redirect = Redirect<Self::Word>;
    type Error = BuilderError;
}

impl<L> M4Builder for MinimalBuilder<L>
where
    L: Into<String> + From<String> + Clone + Debug,
{
    type M4Macro = M4Macro<Self::Command, Self::Word>;
    type M4Argument = M4Argument<Self::Command, Self::Word>;
    fn macro_into_compound_command(
        &mut self,
        macro_call: M4Macro<Self::Command, Self::Word>,
        redirects: Vec<Self::Redirect>,
    ) -> Result<Self::CompoundCommand, Self::Error> {
        let cmd = if let Some(cmd) = self.simplify_m4_macro(&macro_call) {
            cmd
        } else {
            Macro(macro_call)
        };
        let compound_cmd = if !redirects.is_empty() {
            Shell(CompoundCommand::Redirect(
                Box::new(AcCommand::new_may_m4(cmd)),
                redirects,
            ))
        } else {
            cmd
        };
        Ok(compound_cmd)
    }

    fn macro_into_word(
        &mut self,
        macro_call: Self::M4Macro,
    ) -> Result<Self::WordFragment, Self::Error> {
        Ok(Macro(macro_call))
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

impl<L> ShellBuilder for MinimalBuilder<L>
where
    L: Into<String> + From<String> + Clone + Debug,
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
        let mut raw_cmd = list;
        let comments = (!pre_cmd_comments.is_empty())
            .then_some(pre_cmd_comments.into_iter().filter_map(|n| n.0).collect())
            .filter(|c: &String| !c.is_empty());
        let complete_cmd = {
            raw_cmd.comment = comments;
            raw_cmd.range = Some(range);
            raw_cmd
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
                        cmd = AcCommand::new(
                            Compound(Shell(CompoundCommand::And(
                                self.make_condition(cmd)?,
                                next.into(),
                            ))),
                            comments,
                        )
                    }
                    AndOr::Or(next) => {
                        cmd = AcCommand::new(
                            Compound(Shell(CompoundCommand::Or(
                                self.make_condition(cmd)?,
                                next.into(),
                            ))),
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
            Ok(AcCommand::new_compound(CompoundCommand::Pipe(
                bang,
                cmds.into_iter().map(|(_, c)| c).collect(),
            )))
        } else if bang {
            // FIXME Suppport bang in the case of single command with other than fake Pipe.
            Ok(AcCommand::new_compound(CompoundCommand::Pipe(
                bang,
                vec![cmds.pop().unwrap().1],
            )))
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
                RedirectOrEnvVar::EnvVar(k, v) => Ok(AcCommand::new_assign(
                    k.into(),
                    v.unwrap_or(Word::Empty.into()),
                )),
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
                Ok(AcCommand::new_compound(CompoundCommand::Brace(assignments)))
            }
        } else {
            let mut cmd = AcCommand::new_cmd(cmd_words);
            if !redirects.is_empty() {
                cmd = AcCommand::new_compound(CompoundCommand::Redirect(Box::new(cmd), redirects));
            }
            if assignments.is_empty() {
                Ok(cmd)
            } else {
                let mut cmds = assignments;
                cmds.push(cmd);
                Ok(AcCommand::new_compound(CompoundCommand::Brace(cmds)))
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
        let mut cmd = CompoundCommand::Brace(cmds);
        if !redirects.is_empty() {
            cmd = CompoundCommand::Redirect(Box::new(AcCommand::new_compound(cmd)), redirects)
        }
        Ok(Shell(cmd))
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
        let mut cmd = CompoundCommand::Subshell(cmds);
        if !redirects.is_empty() {
            cmd = CompoundCommand::Redirect(Box::new(AcCommand::new_compound(cmd)), redirects)
        }
        Ok(Shell(cmd))
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
            LoopKind::While => CompoundCommand::While(guard_body_pair),
            LoopKind::Until => CompoundCommand::Until(guard_body_pair),
        };
        if !redirects.is_empty() {
            cmd = CompoundCommand::Redirect(Box::new(AcCommand::new_compound(cmd)), redirects)
        }
        Ok(Shell(cmd))
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

        let mut cmd = CompoundCommand::If {
            conditionals,
            else_branch,
        };
        if !redirects.is_empty() {
            cmd = CompoundCommand::Redirect(Box::new(AcCommand::new_compound(cmd)), redirects)
        }
        Ok(Shell(cmd))
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

        let mut cmd = CompoundCommand::For {
            var: fragments.var,
            words,
            body,
        };
        if !redirects.is_empty() {
            cmd = CompoundCommand::Redirect(Box::new(AcCommand::new_compound(cmd)), redirects)
        }
        Ok(Shell(cmd))
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

        let mut cmd = CompoundCommand::Case {
            word: fragments.word,
            arms,
        };
        if !redirects.is_empty() {
            cmd = CompoundCommand::Redirect(Box::new(AcCommand::new_compound(cmd)), redirects)
        }
        Ok(Shell(cmd))
    }

    /// Converts a `CompoundCommand` into a `PipeableCommand`.
    fn compound_command_into_pipeable(
        &mut self,
        cmd: Self::CompoundCommand,
    ) -> Result<Self::PipeableCommand, Self::Error> {
        Ok(AcCommand::new_may_m4(cmd))
    }

    /// Constructs a `Command::FunctionDef` node with the provided inputs.
    fn function_declaration(
        &mut self,
        name: String,
        _post_name_comments: Vec<Newline>,
        body: Self::CompoundCommand,
    ) -> Result<Self::PipeableCommand, Self::Error> {
        Ok(AcCommand::new_compound(CompoundCommand::FunctionDef {
            name,
            body: Box::new(AcCommand::new_may_m4(body)),
        }))
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
        Ok(Shell(simple))
    }

    /// Constructs a `ast::Word` from the provided input.
    fn word(
        &mut self,
        kind: ConcatWordKind<Self::WordFragment>,
    ) -> Result<Self::Word, Self::Error> {
        let map_quote_word = |kind: QuoteWordKind<MayM4<WordFragment<L, _, _>, M4Macro<_, _>>>| {
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

        let word = match compress(kind) {
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

        Ok(AcWord(word))
    }

    /// Constructs a `ast::Redirect` from the provided input.
    fn redirect(&mut self, kind: RedirectKind<Self::Word>) -> Result<Self::Redirect, Self::Error> {
        use super::RedirectKind::*;
        let io = match kind {
            Read(fd, path) => Redirect::Read(fd, path),
            Write(fd, path) => Redirect::Write(fd, path),
            ReadWrite(fd, path) => Redirect::ReadWrite(fd, path),
            Append(fd, path) => Redirect::Append(fd, path),
            Clobber(fd, path) => Redirect::Clobber(fd, path),
            Heredoc(fd, body) => Redirect::Heredoc(fd, body),
            DupRead(src, dst) => Redirect::DupRead(src, dst),
            DupWrite(src, dst) => Redirect::DupWrite(src, dst),
        };

        Ok(io)
    }
}

/// A trait which provides ability to parse test commands in detail
pub trait ConditionBuilder: BuilderBase
where
    Self::Word: From<String> + Into<Option<String>> + Clone,
{
    /// construct Condition
    fn make_condition(
        &mut self,
        cmd: Self::Command,
    ) -> Result<Condition<Self::Command, Self::Word>, Self::Error>;

    /// parses test command
    fn parse_test_command(
        &self,
        words: &[Self::Word],
    ) -> Result<Condition<Self::Command, Self::Word>, Self::Error> {
        #[derive(Debug)]
        enum OperatorKind {
            Eq,
            Neq,
            Ge,
            Gt,
            Le,
            Lt,
            Empty,
            NonEmpty,
            Dir,
            File,
            NoExists,
        }
        #[derive(Debug)]
        enum ConcatKind {
            And,
            Or,
        }
        #[derive(Debug)]
        struct ExpressionState<W> {
            flipped: bool,
            operator: Option<OperatorKind>,
            lhs: W,
            rhs: W,
        }
        use ConcatKind::*;
        use OperatorKind::*;

        fn make_operator<W>(state: ExpressionState<W>) -> Operator<W> {
            if let Some(operator) = state.operator {
                match operator {
                    Neq => Operator::Neq(state.lhs, state.rhs),
                    Eq => Operator::Eq(state.lhs, state.rhs),
                    Ge => Operator::Ge(state.lhs, state.rhs),
                    Gt => Operator::Gt(state.lhs, state.rhs),
                    Le => Operator::Le(state.lhs, state.rhs),
                    Lt => Operator::Lt(state.lhs, state.rhs),
                    Empty => Operator::Empty(state.rhs),
                    NonEmpty => Operator::NonEmpty(state.rhs),
                    Dir => Operator::Dir(state.rhs),
                    File => Operator::File(state.rhs),
                    NoExists => Operator::NoExists(state.rhs),
                }
            } else {
                Operator::NonEmpty(state.lhs)
            }
        }

        let empty: Self::Word = String::new().into();
        let mut state = ExpressionState {
            flipped: false,
            operator: None,
            lhs: empty.clone(),
            rhs: empty.clone(),
        };

        let mut conditions: Vec<Condition<_, _>> = Vec::new();
        let mut concat_kinds: Vec<ConcatKind> = Vec::new();
        for word in words {
            if state.operator.is_none() {
                if let Some(literal) = word.clone().into() {
                    match literal.as_str() {
                        "!=" | "-ne" => {
                            state.operator.replace(Neq);
                            continue;
                        }
                        "=" | "-eq" => {
                            state.operator.replace(Eq);
                            continue;
                        }
                        "-ge" => {
                            state.operator.replace(Ge);
                            continue;
                        }
                        "-gt" => {
                            state.operator.replace(Gt);
                            continue;
                        }
                        "-le" => {
                            state.operator.replace(Le);
                            continue;
                        }
                        "-lt" => {
                            state.operator.replace(Lt);
                            continue;
                        }
                        "-z" => {
                            state.operator.replace(Empty);
                            continue;
                        }
                        "-n" => {
                            state.operator.replace(NonEmpty);
                            continue;
                        }
                        "-d" | "-f" if state.flipped => {
                            state.operator.replace(NoExists);
                            continue;
                        }
                        "-d" => {
                            state.operator.replace(Dir);
                            continue;
                        }
                        // TODO: More precision for file existance + extra operators
                        "-f" | "-e" | "-g" | "-G" | "-h" | "-k" | "-L" | "-N" | "-O" | "-p"
                        | "-r" | "-s" | "-S" | "-u" | "-w" | "-x" => {
                            state.operator.replace(File);
                            continue;
                        }
                        "!" => {
                            state.flipped = true;
                            continue;
                        }
                        _ => {
                            // return Err(BuilderError::UnsupportedSyntax);
                        }
                    }
                }
                // before encountering an operator
                // and the word is not an operator
                state.lhs = word.clone();
            } else {
                if let Some(literal) = word.clone().into() {
                    match literal.as_str() {
                        "-a" => {
                            conditions.push(Condition::Cond(make_operator(state)));
                            concat_kinds.push(And);
                            state = ExpressionState {
                                flipped: false,
                                operator: None,
                                lhs: empty.clone(),
                                rhs: empty.clone(),
                            };
                            continue;
                        }
                        "-o" => {
                            conditions.push(Condition::Cond(make_operator(state)));
                            concat_kinds.push(Or);
                            state = ExpressionState {
                                operator: None,
                                lhs: empty.clone(),
                                rhs: empty.clone(),
                                flipped: false,
                            };
                            continue;
                        }
                        _ => {}
                    }
                }
                state.rhs = word.clone();
            }
        }
        conditions.push(Condition::Cond(make_operator(state)));
        debug_assert!(conditions.len() == concat_kinds.len() + 1);
        let mut ret = conditions.remove(0);
        for concat_kind in concat_kinds {
            match concat_kind {
                And => ret = Condition::And(Box::new(ret), Box::new(conditions.remove(0))),
                Or => ret = Condition::Or(Box::new(ret), Box::new(conditions.remove(0))),
            }
        }
        Ok(ret)
    }
}

impl<L> ConditionBuilder for MinimalBuilder<L>
where
    L: From<String> + Into<String> + Clone + Debug,
{
    fn make_condition(
        &mut self,
        cmd: MinimalCommand<L>,
    ) -> Result<Condition<AcCommand<L, AcWord<L>>, AcWord<L>>, BuilderError> {
        match cmd.cmd.as_ref() {
            Cmd(words) => {
                let first_word = words.first().unwrap();
                if let Word::Single(w) = &first_word.0 {
                    match w {
                        Shell(WordFragment::Literal(v)) if v.clone().into() == "test" => {
                            Some(self.parse_test_command(&words[1..])?)
                        }
                        Shell(WordFragment::Literal(v)) if v.clone().into() == "eval" => Some(
                            Condition::Eval(Box::new(AcCommand::new_cmd(words[1..].to_owned()))),
                        ),
                        Shell(WordFragment::Subst(s)) => match s.as_ref() {
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
            Compound(Shell(CompoundCommand::And(first, second))) => Some(Condition::And(
                Box::new(first.clone()),
                Box::new(self.make_condition(*second.clone())?),
            )),
            Compound(Shell(CompoundCommand::Or(first, second))) => Some(Condition::Or(
                Box::new(first.clone()),
                Box::new(self.make_condition(*second.clone())?),
            )),
            Compound(Shell(CompoundCommand::Pipe(true, cmds))) if cmds.len() == 1 => {
                let cmd = cmds.first().unwrap().clone();
                self.make_condition(cmd).ok().map(|cond| cond.flip())
            }
            _ => None,
        }
        .map_or(Ok(Condition::ReturnZero(Box::new(cmd))), Ok)
    }
}

pub(crate) fn compress<L, C, W, F>(word: ConcatWordKind<F>) -> ConcatWordKind<F>
where
    F: From<WordFragment<L, C, W>> + Into<Option<WordFragment<L, C, W>>> + Clone,
    L: Into<String> + From<String>,
{
    fn coalesce_word<L, C, W, F>(a: F, b: F) -> CoalesceResult<F>
    where
        F: From<WordFragment<L, C, W>> + Into<Option<WordFragment<L, C, W>>> + Clone,
        L: Into<String> + From<String>,
    {
        use crate::ast::minimal::WordFragment::Literal;
        match (a.clone().into(), b.clone().into()) {
            (Some(Literal(a)), Some(Literal(b))) => {
                let (mut a, b) = (a.into(), b.into());
                a.push_str(&b);
                Ok(Literal(a.into()).into())
            }
            _ => Err((a, b)),
        }
    }

    fn coalesce_quote_word<L, C, W, F>(
        a: QuoteWordKind<F>,
        b: QuoteWordKind<F>,
    ) -> CoalesceResult<QuoteWordKind<F>>
    where
        F: From<WordFragment<L, C, W>> + Into<Option<WordFragment<L, C, W>>> + Clone,
        L: Into<String> + From<String>,
    {
        use QuoteWordKind::*;
        match (a, b) {
            (Simple(a), Simple(b)) => coalesce_word(a, b)
                .map(|s| Simple(s))
                .map_err(|(a, b)| (Simple(a), Simple(b))),
            (SingleQuoted(mut a), SingleQuoted(b)) => {
                a.push_str(&b);
                Ok(SingleQuoted(a))
            }
            (DoubleQuoted(a), DoubleQuoted(b)) => {
                let quoted =
                    Coalesce::new(a.into_iter().chain(b), |a, b| coalesce_word(a, b)).collect();
                Ok(DoubleQuoted(quoted))
            }
            (a, b) => Err((a, b)),
        }
    }

    use crate::ast::builder::ConcatWordKind::*;
    use QuoteWordKind::*;
    match word {
        Single(quote_kind) => Single(match quote_kind {
            s @ Simple(_) => s,
            DoubleQuoted(v) => DoubleQuoted(Coalesce::new(v, coalesce_word).collect()),
            s => s,
        }),
        Concat(v) => {
            let mut body: Vec<_> = Coalesce::new(v, coalesce_quote_word).collect();
            if body.len() == 1 {
                Single(body.pop().unwrap())
            } else {
                Concat(body)
            }
        }
    }
}

pub(super) trait M4Simplifier: ShellBuilder
where
    Self::Command: Clone,
    Self::Word: Clone,
{
    fn simplify_m4_macro(
        &mut self,
        macro_call: &M4Macro<Self::Command, Self::Word>,
    ) -> Option<Self::CompoundCommand> {
        match macro_call.name.as_str() {
            "AS_IF" if macro_call.args.len() > 0 => {
                let guard = CommandGroup {
                    commands: macro_call.get_arg_as_cmd(0).unwrap_or_default(),
                    trailing_comments: Default::default(),
                };
                let body = CommandGroup {
                    commands: macro_call.get_arg_as_cmd(1).unwrap_or_default(),
                    trailing_comments: Default::default(),
                };
                let conditionals = vec![GuardBodyPairGroup { guard, body }];
                let else_branch = macro_call.get_arg_as_cmd(2).map(|commands| CommandGroup {
                    commands,
                    trailing_comments: Default::default(),
                });
                self.if_command(
                    IfFragments {
                        conditionals,
                        else_branch,
                    },
                    vec![],
                )
                .ok()
            }
            "AS_CASE" if macro_call.args.len() > 0 => {
                let word = macro_call
                    .get_arg_as_word(0)
                    .expect("Invalid AS_CASE syntax detected.");
                let mut pattern_iter = macro_call.args.iter().skip(1);
                let mut action_iter = macro_call.args.iter().skip(2);
                let mut arms = Vec::new();
                loop {
                    match (pattern_iter.next(), action_iter.next()) {
                        (Some(M4Argument::Word(pattern)), Some(M4Argument::Commands(action))) => {
                            arms.push(CaseArm {
                                patterns: CasePatternFragments {
                                    pre_pattern_comments: Default::default(),
                                    pattern_alternatives: vec![pattern.clone()],
                                    pattern_comment: Default::default(),
                                },
                                body: CommandGroup {
                                    commands: action.clone(),
                                    trailing_comments: Default::default(),
                                },
                                arm_comment: Default::default(),
                            });
                        }
                        (Some(M4Argument::Commands(default_action)), None) => {
                            let kind = self.word_fragment(WordKind::Star).ok()?;
                            let star = self
                                .word(ConcatWordKind::Single(QuoteWordKind::Simple(kind)))
                                .ok()?;
                            arms.push(CaseArm {
                                patterns: CasePatternFragments {
                                    pre_pattern_comments: Default::default(),
                                    pattern_alternatives: vec![star],
                                    pattern_comment: Default::default(),
                                },
                                body: CommandGroup {
                                    commands: default_action.clone(),
                                    trailing_comments: Default::default(),
                                },
                                arm_comment: Default::default(),
                            });
                            break;
                        }
                        _ => break,
                    }
                }
                self.case_command(
                    CaseFragments {
                        word,
                        post_word_comments: Default::default(),
                        in_comment: Default::default(),
                        arms,
                        post_arms_comments: Default::default(),
                    },
                    Default::default(),
                )
                .ok()
            }
            _ => None,
        }
    }
}

impl<L> M4Simplifier for MinimalBuilder<L> where L: Into<String> + From<String> + Clone + Debug {}
