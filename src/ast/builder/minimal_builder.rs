use std::fmt::Debug;
use std::marker::PhantomData;

use super::{
    Builder, BuilderError, CaseFragments, CommandGroup, ComplexWordKind, ForFragments, GuardBodyPairGroup,
    IfFragments, LoopKind, Newline, RedirectKind, SeparatorKind, SimpleWordKind
};

use crate::ast::minimal::Command::*;
use crate::{
    ast::{
        builder::compress,
        minimal::{
            CommandWrapper, CompoundCommand, Condition, GuardBodyPair, Operator, Word, WordFragment,
        },
        AndOr, Arithmetic, DefaultArithmetic, DefaultParameter, Parameter, ParameterSubstitution,
        PatternBodyPair, Redirect, RedirectOrCmdWord, RedirectOrEnvVar,
    },
    m4_macro::{M4Argument, M4Macro, SideEffect},
};

/// TODO: add doc comments
#[derive(Debug, Default, PartialEq, Eq, Clone)]
pub struct MinimalBuilder<T> {
    phantom_data: PhantomData<T>,
}

impl<T> Builder for MinimalBuilder<T>
where
    T: Into<String> + From<String> + Clone + Debug,
{
    type Command = CommandWrapper<T>;
    type CommandList = Self::Command;
    type ListableCommand = Self::Command;
    type PipeableCommand = Self::Command;
    type CompoundCommand = CompoundCommand<Self::Word, Self::Command>;
    type Word = Word<T, Self::Command>;
    type Redirect = Redirect<Self::Word>;
    type Error = BuilderError;
    type M4Macro = M4Macro<Self::Word, Self::Command>;
    type M4Argument = M4Argument<Self::Word, Self::Command>;

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
                        cmd = CommandWrapper::new_with_comment(
                            Compound(CompoundCommand::And(make_condition(cmd)?, next.into())),
                            comments,
                        )
                    }
                    AndOr::Or(next) => {
                        cmd = CommandWrapper::new_with_comment(
                            Compound(CompoundCommand::Or(make_condition(cmd)?, next.into())),
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
            Ok(CommandWrapper::new(Compound(CompoundCommand::Pipe(
                bang,
                cmds.into_iter().map(|(_, c)| c).collect(),
            ))))
        } else if bang {
            // FIXME Suppport bang in the case of single command with other than fake Pipe.
            Ok(CommandWrapper::new(Compound(CompoundCommand::Pipe(
                bang,
                vec![cmds.pop().unwrap().1],
            ))))
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
                RedirectOrEnvVar::EnvVar(k, v) => Ok(CommandWrapper::new(Assignment(
                    k.into(),
                    v.unwrap_or(Word::Empty),
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
                Ok(CommandWrapper::new(Compound(CompoundCommand::Brace(
                    assignments,
                ))))
            }
        } else {
            let mut cmd = CommandWrapper::new(Cmd(cmd_words));
            if !redirects.is_empty() {
                cmd = CommandWrapper::new(Compound(CompoundCommand::Redirect(
                    Box::new(cmd),
                    redirects,
                )));
            }
            if assignments.is_empty() {
                Ok(cmd)
            } else {
                assignments.push(cmd);
                Ok(CommandWrapper::new(Compound(CompoundCommand::Subshell(
                    assignments,
                ))))
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
            cmd = CompoundCommand::Redirect(Box::new(CommandWrapper::new(Compound(cmd))), redirects)
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
        let mut cmd = CompoundCommand::Subshell(cmds);
        if !redirects.is_empty() {
            cmd = CompoundCommand::Redirect(Box::new(CommandWrapper::new(Compound(cmd))), redirects)
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
            condition: make_condition(guard.pop().unwrap())?,
            body,
        };

        let mut cmd = match kind {
            LoopKind::While => CompoundCommand::While(guard_body_pair),
            LoopKind::Until => CompoundCommand::Until(guard_body_pair),
        };
        if !redirects.is_empty() {
            cmd = CompoundCommand::Redirect(Box::new(CommandWrapper::new(Compound(cmd))), redirects)
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
                    condition: make_condition(guard.pop().unwrap())?,
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
            cmd = CompoundCommand::Redirect(Box::new(CommandWrapper::new(Compound(cmd))), redirects)
        }
        Ok(cmd)
    }

    /// Constructs a `CompoundCommand::For` node with the provided inputs.
    fn for_command(
        &mut self,
        fragments: ForFragments<Self::Word, Self::Command>,
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
            cmd = CompoundCommand::Redirect(Box::new(CommandWrapper::new(Compound(cmd))), redirects)
        }
        Ok(cmd)
    }

    /// Constructs a `CompoundCommand::Case` node with the provided inputs.
    fn case_command(
        &mut self,
        fragments: CaseFragments<Self::Word, Self::Command>,
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

        let mut cmd = CompoundCommand::Case {
            word: fragments.word,
            arms,
        };
        if !redirects.is_empty() {
            cmd = CompoundCommand::Redirect(Box::new(CommandWrapper::new(Compound(cmd))), redirects)
        }
        Ok(cmd)
    }

    /// Converts a `CompoundCommand` into a `PipeableCommand`.
    fn compound_command_into_pipeable(
        &mut self,
        cmd: Self::CompoundCommand,
    ) -> Result<Self::PipeableCommand, Self::Error> {
        Ok(CommandWrapper::new(Compound(cmd)))
    }

    /// Constructs a `Command::FunctionDef` node with the provided inputs.
    fn function_declaration(
        &mut self,
        name: String,
        _post_name_comments: Vec<Newline>,
        body: Self::CompoundCommand,
    ) -> Result<Self::PipeableCommand, Self::Error> {
        Ok(CommandWrapper::new(Compound(
            CompoundCommand::FunctionDef {
                name,
                body: Box::new(CommandWrapper::new(Compound(body))),
            },
        )))
    }

    fn macro_into_compound_command(
        &mut self,
        macro_call: M4Macro<Self::Word, Self::Command>,
        redirects: Vec<Self::Redirect>,
    ) -> Result<Self::CompoundCommand, Self::Error> {
        let mut cmd = CompoundCommand::Macro(macro_call);
        if !redirects.is_empty() {
            cmd = CompoundCommand::Redirect(Box::new(CommandWrapper::new(Compound(cmd))), redirects)
        }
        Ok(cmd)
    }

    fn macro_into_word(
        &mut self,
        macro_call: Self::M4Macro,
    ) -> Result<SimpleWordKind<Self::Command, Self::M4Macro>, Self::Error> {
        Ok(SimpleWordKind::Macro(
            macro_call.name.to_string(),
            macro_call,
        ))
    }

    fn macro_call(
        &mut self,
        name: String,
        args: Vec<M4Argument<Self::Word, Self::Command>>,
        effects: Option<SideEffect>,
        original_name: Option<String>,
    ) -> Result<Self::M4Macro, Self::Error> {
        Ok(M4Macro::new_with_side_effect(name, args, effects, original_name))
    }

    /// Ignored by the builder.
    fn comments(&mut self, _comments: Vec<Newline>) -> Result<(), Self::Error> {
        Ok(())
    }

    /// Constructs a `ast::Word` from the provided input.
    fn word(
        &mut self,
        kind: ComplexWordKind<Self::Command, Self::M4Macro>,
    ) -> Result<Self::Word, Self::Error> {
        macro_rules! map {
            ($pat:expr) => {
                match $pat {
                    Some(w) => Some(self.word(w)?),
                    None => None,
                }
            };
        }

        fn map_arith<T: From<String>>(kind: DefaultArithmetic) -> Arithmetic<T> {
            use crate::ast::Arithmetic::*;
            match kind {
                Var(v) => Var(v.into()),
                Literal(l) => Literal(l),
                Pow(a, b) => Pow(Box::new(map_arith(*a)), Box::new(map_arith(*b))),
                PostIncr(p) => PostIncr(p.into()),
                PostDecr(p) => PostDecr(p.into()),
                PreIncr(p) => PreIncr(p.into()),
                PreDecr(p) => PreDecr(p.into()),
                UnaryPlus(a) => UnaryPlus(Box::new(map_arith(*a))),
                UnaryMinus(a) => UnaryMinus(Box::new(map_arith(*a))),
                LogicalNot(a) => LogicalNot(Box::new(map_arith(*a))),
                BitwiseNot(a) => BitwiseNot(Box::new(map_arith(*a))),
                Mult(a, b) => Mult(Box::new(map_arith(*a)), Box::new(map_arith(*b))),
                Div(a, b) => Div(Box::new(map_arith(*a)), Box::new(map_arith(*b))),
                Modulo(a, b) => Modulo(Box::new(map_arith(*a)), Box::new(map_arith(*b))),
                Add(a, b) => Add(Box::new(map_arith(*a)), Box::new(map_arith(*b))),
                Sub(a, b) => Sub(Box::new(map_arith(*a)), Box::new(map_arith(*b))),
                ShiftLeft(a, b) => ShiftLeft(Box::new(map_arith(*a)), Box::new(map_arith(*b))),
                ShiftRight(a, b) => ShiftRight(Box::new(map_arith(*a)), Box::new(map_arith(*b))),
                Less(a, b) => Less(Box::new(map_arith(*a)), Box::new(map_arith(*b))),
                LessEq(a, b) => LessEq(Box::new(map_arith(*a)), Box::new(map_arith(*b))),
                Great(a, b) => Great(Box::new(map_arith(*a)), Box::new(map_arith(*b))),
                GreatEq(a, b) => GreatEq(Box::new(map_arith(*a)), Box::new(map_arith(*b))),
                Eq(a, b) => Eq(Box::new(map_arith(*a)), Box::new(map_arith(*b))),
                NotEq(a, b) => NotEq(Box::new(map_arith(*a)), Box::new(map_arith(*b))),
                BitwiseAnd(a, b) => BitwiseAnd(Box::new(map_arith(*a)), Box::new(map_arith(*b))),
                BitwiseXor(a, b) => BitwiseXor(Box::new(map_arith(*a)), Box::new(map_arith(*b))),
                BitwiseOr(a, b) => BitwiseOr(Box::new(map_arith(*a)), Box::new(map_arith(*b))),
                LogicalAnd(a, b) => LogicalAnd(Box::new(map_arith(*a)), Box::new(map_arith(*b))),
                LogicalOr(a, b) => LogicalOr(Box::new(map_arith(*a)), Box::new(map_arith(*b))),
                Ternary(a, b, c) => Ternary(
                    Box::new(map_arith(*a)),
                    Box::new(map_arith(*b)),
                    Box::new(map_arith(*c)),
                ),
                Assign(v, a) => Assign(v.into(), Box::new(map_arith(*a))),
                Sequence(ariths) => Sequence(ariths.into_iter().map(map_arith).collect()),
            }
        }

        let map_param = |kind: DefaultParameter| -> Parameter<T> {
            use crate::ast::Parameter::*;
            match kind {
                At => At,
                Star => Star,
                Pound => Pound,
                Question => Question,
                Dash => Dash,
                Dollar => Dollar,
                Bang => Bang,
                Positional(p) => Positional(p),
                Var(v) => Var(v.into()),
            }
        };

        let mut map_simple = |kind| {
            use super::ParameterSubstitutionKind::*;
            use super::SimpleWordKind::*;

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
                Macro(_, m) => WordFragment::Macro(m),

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
                        Default(c, p, w) => {
                            ParameterSubstitution::Default(c, map_param(p), map!(w))
                        }
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
            Ok(simple)
        };

        let mut map_word = |kind| {
            use super::SimpleWordKind::*;
            use super::WordKind::*;
            let word = match kind {
                Simple(Literal(s)) | SingleQuoted(s)
                    if s.is_empty() =>
                {
                    None // empty string
                }
                Simple(s) => Some(map_simple(s)?),
                SingleQuoted(s) => Some(WordFragment::Literal(s.into())),
                DoubleQuoted(mut v) => match v.len() {
                    0 => None,                                // empty string
                    1 => Some(map_simple(v.pop().unwrap())?), // like "foo", "${foo}"
                    _ => Some(WordFragment::DoubleQuoted(
                        v.into_iter()
                            .map(&mut map_simple)
                            .collect::<Result<Vec<_>, _>>()?,
                    )),
                },
            };
            Ok(word)
        };

        let word = match compress(kind) {
            ComplexWordKind::Single(s) => map_word(s)?.map_or(Word::Empty, |w| Word::Single(w)),
            ComplexWordKind::Concat(words) => Word::Concat(
                words
                    .into_iter()
                    .map(map_word)
                    .collect::<Result<Vec<_>, _>>()?
                    .into_iter()
                    .flatten()
                    .collect(),
            ),
        };

        Ok(word)
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

pub(crate) fn parse_condition<T, C>(words: &[Word<T, C>]) -> Result<Operator<Word<T, C>>, BuilderError>
where
    T: Into<String> + Clone,
    Word<T, C>: Clone,
{
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
    use OperatorKind::*;

    let mut lhs = Word::Empty;
    let mut rhs = Word::Empty;
    let mut operator_kind = None;
    let mut flipped = false;
    for word in words {
        if operator_kind.is_none() {
            if let Word::Single(w) = &word {
                if let WordFragment::Literal(literal) = w {
                    operator_kind = match literal.clone().into().as_str() {
                        "!" => {
                            flipped = true;
                            continue;
                        }
                        "!=" | "-ne" => Some(Neq),
                        "=" | "-eq" => Some(Eq),
                        "-ge" => Some(Ge),
                        "-gt" => Some(Gt),
                        "-le" => Some(Le),
                        "-lt" => Some(Lt),
                        "-z" => Some(Empty),
                        "-n" => Some(NonEmpty),
                        "-d" | "-f" if flipped => Some(NoExists),
                        "-d" => Some(Dir),
                        // TODO: More precision for file existance + extra operators
                        "-f" | "-e" | "-g" | "-G" | "-h" | "-k" | "-L" | "-N" | "-O" | "-p"
                        | "-r" | "-s" | "-S" | "-u" | "-w" | "-x" => Some(File),
                        _ => {
                            // return Err(BuilderError::UnsupportedSyntax);
                            None
                        }
                    };
                    if operator_kind.is_some() {
                        continue;
                    }
                }
            }
            // before encountering an operator
            // and the word is not an operator
            lhs = word.clone();
        } else {
            rhs = word.clone();
        }
    }
    let operator = match operator_kind.unwrap() {
        Neq => Operator::Neq(lhs, rhs),
        Eq => Operator::Eq(lhs, rhs),
        Ge => Operator::Ge(lhs, rhs),
        Gt => Operator::Gt(lhs, rhs),
        Le => Operator::Le(lhs, rhs),
        Lt => Operator::Lt(lhs, rhs),
        Empty => Operator::Empty(rhs),
        NonEmpty => Operator::NonEmpty(rhs),
        Dir => Operator::Dir(rhs),
        File => Operator::File(rhs),
        NoExists => Operator::NoExists(rhs),
    };
    Ok(operator)
}

type MinimalWord<V> = Word<V, CommandWrapper<V>>;

fn make_condition<V>(
    cmd: CommandWrapper<V>,
) -> Result<Condition<MinimalWord<V>, CommandWrapper<V>>, BuilderError>
where
    V: Into<String> + Clone + Debug,
{
    match &cmd.cmd {
        Cmd(words) => {
            let first_word = words.first().unwrap();
            if let Word::Single(w) = first_word {
                match w {
                    WordFragment::Literal(v) if v.clone().into() == "test" => {
                        Some(Condition::Cond(parse_condition(&words[1..])?))
                    }
                    WordFragment::Literal(v) if v.clone().into() == "eval" => Some(
                        Condition::Eval(vec![CommandWrapper::new(Cmd(words[1..].to_owned()))]),
                    ),
                    WordFragment::Subst(s) => match s.as_ref() {
                        ParameterSubstitution::Command(cmds) => Some(Condition::Eval(cmds.clone())),
                        _ => None,
                    },
                    _ => None,
                }
            } else {
                None
            }
        }
        Compound(CompoundCommand::And(first, second)) => Some(Condition::And(
            Box::new(first.clone()),
            Box::new(make_condition(*second.clone())?),
        )),
        Compound(CompoundCommand::Or(first, second)) => Some(Condition::Or(
            Box::new(first.clone()),
            Box::new(make_condition(*second.clone())?),
        )),
        _ => None,
    }
    .map_or(Ok(Condition::ReturnZero(Box::new(cmd))), Ok)
}
