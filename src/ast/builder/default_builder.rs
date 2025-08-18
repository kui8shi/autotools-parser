use crate::ast::builder::{
    BuilderBase, Coalesce, CoalesceResult, M4Builder, QuoteWordKind, ShellBuilder, WordKind,
};
use crate::ast::{
    map_arith, map_param, AndOr, AndOrList, AtomicShellPipeableCommand, AtomicTopLevelCommand,
    AtomicTopLevelWord, Command, ComplexWord, CompoundCommand, CompoundCommandKind, GuardBodyPair,
    ListableCommand, M4Word, MayM4, ParameterSubstitution, PatternBodyPair, PipeableCommand,
    Redirect, RedirectOrCmdWord, RedirectOrEnvVar, ShellCompoundCommand, ShellPipeableCommand,
    ShellWord, SimpleCommand, SimpleWord, TopLevelCommand, TopLevelWord, Word,
};
use crate::m4_macro::{M4Argument, M4Macro, SideEffect};
use std::default::Default;
use std::fmt;
use std::marker::PhantomData;
use std::rc::Rc;
use std::sync::Arc;
use void::Void;

use super::{
    CaseFragments, CommandGroup, ConcatWordKind, ForFragments, GuardBodyPairGroup, IfFragments,
    LoopKind, Newline, RedirectKind, SeparatorKind,
};

/// A macro for defining a default builder, its boilerplate, and delegating
/// the `Builder` trait to its respective `CoreBuilder` type.
///
/// This allows us to create concrete atomic/non-atomic builders which
/// wrap a concrete `CoreBuilder` implementation so we can hide its type
/// complexity from the consumer.
///
/// We could accomplish this by using a public type alias to the private
/// builder type, however, rustdoc will only generate docs for the alias
/// definition is, and the docs for the inner builder will be rendered in
/// their entire complexity.
// FIXME: might be good to revisit this complexity/indirection
macro_rules! default_builder {
    ($(#[$attr:meta])*
     pub struct $Builder:ident,
     $CoreBuilder:ident,
     $Word:ident,
     $WordFragment:ident,
     $Cmd:ident,
     $PipeableCmd:ident,
    ) => {
        $(#[$attr])*
        pub struct $Builder<T>($CoreBuilder<T, $Cmd<T>, $Word<T>>);

        impl<T> $Builder<T> {
            /// Constructs a builder.
            pub fn new() -> Self {
                $Builder($CoreBuilder::new())
            }
        }

        impl<T> fmt::Debug for $Builder<T> {
            fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
                fmt.debug_struct(stringify!($Builder))
                    .finish()
            }
        }

        impl<T> Default for $Builder<T> {
            fn default() -> Self {
                Self::new()
            }
        }

        impl<T> Clone for $Builder<T> {
            fn clone(&self) -> Self {
                *self
            }
        }

        impl<T> Copy for $Builder<T> {}

        impl<T: Into<String> + From<String>> BuilderBase for $Builder<T> {
            type Command         = $Cmd<T>;
            type CompoundCommand = ShellCompoundCommand<T, Self::Command, Self::Word>;
            type Word            = $Word<T>;
            type WordFragment    = $WordFragment<T, Self::Command, Self::Word>;
            type Redirect        = Redirect<Self::Word>;
            type Error           = Void;
        }

        impl<T: Into<String> + From<String>> M4Builder for $Builder<T> {
            type M4Macro         = M4Macro<Self::Command, Self::Word>;
            type M4Argument      = M4Argument<Self::Command, Self::Word>;
            fn macro_into_compound_command(
                &mut self,
                macro_call: M4Macro<Self::Command, Self::Word>,
                redirects: Vec<Self::Redirect>,
            ) -> Result<Self::CompoundCommand, Self::Error> {
                self.0.macro_into_compound_command(macro_call, redirects)
            }

            fn macro_into_word(&mut self,
                               macro_call: Self::M4Macro
            ) -> Result<Self::WordFragment, Self::Error> {
                self.0.macro_into_word(macro_call)
            }

            fn macro_call(
                &mut self,
                name: String,
                args: Vec<M4Argument<Self::Command, Self::Word>>,
                effects: Option<SideEffect>,
                original_name: Option<String>,
            ) -> Result<Self::M4Macro, Self::Error> {
                self.0.macro_call(name, args, effects, original_name)
            }


        }

        impl<T: Into<String> + From<String>> ShellBuilder for $Builder<T> {
            type CommandList     = AndOrList<Self::ListableCommand>;
            type ListableCommand = ListableCommand<Self::PipeableCommand>;
            type PipeableCommand = $PipeableCmd<T, Self::Command, Self::Word>;

            fn complete_command(&mut self,
                                pre_cmd_comments: Vec<Newline>,
                                list: Self::CommandList,
                                separator: SeparatorKind,
                                cmd_comment: Option<Newline>,
                                range: (usize, usize),
            )
                -> Result<Self::Command, Self::Error>
            {
                self.0.complete_command(pre_cmd_comments, list, separator, cmd_comment, range)
            }

            fn and_or_list(&mut self,
                      first: Self::ListableCommand,
                      rest: Vec<(Vec<Newline>, AndOr<Self::ListableCommand>)>)
                -> Result<Self::CommandList, Self::Error>
            {
                self.0.and_or_list(first, rest)
            }

            fn pipeline(&mut self,
                        bang: bool,
                        cmds: Vec<(Vec<Newline>, Self::PipeableCommand)>)
                -> Result<Self::ListableCommand, Self::Error>
            {
                self.0.pipeline(bang, cmds)
            }

            fn simple_command(
                &mut self,
                redirects_or_env_vars: Vec<RedirectOrEnvVar<Self::Redirect, String, Self::Word>>,
                redirects_or_cmd_words: Vec<RedirectOrCmdWord<Self::Redirect, Self::Word>>
            ) -> Result<Self::PipeableCommand, Self::Error>
            {
                self.0.simple_command(redirects_or_env_vars, redirects_or_cmd_words)
            }

            fn brace_group(&mut self,
                           cmds: CommandGroup<Self::Command>,
                           redirects: Vec<Self::Redirect>)
                -> Result<Self::CompoundCommand, Self::Error>
            {
                self.0.brace_group(cmds, redirects)
            }

            fn subshell(&mut self,
                        cmds: CommandGroup<Self::Command>,
                        redirects: Vec<Self::Redirect>)
                -> Result<Self::CompoundCommand, Self::Error>
            {
                self.0.subshell(cmds, redirects)
            }

            fn loop_command(&mut self,
                            kind: LoopKind,
                            guard_body_pair: GuardBodyPairGroup<Self::Command>,
                            redirects: Vec<Self::Redirect>)
                -> Result<Self::CompoundCommand, Self::Error>
            {
                self.0.loop_command(kind, guard_body_pair, redirects)
            }

            fn if_command(&mut self,
                          fragments: IfFragments<Self::Command>,
                          redirects: Vec<Self::Redirect>)
                -> Result<Self::CompoundCommand, Self::Error>
            {
                self.0.if_command(fragments, redirects)
            }

            fn for_command(&mut self,
                           fragments: ForFragments<Self::Command, Self::Word>,
                           redirects: Vec<Self::Redirect>)
                -> Result<Self::CompoundCommand, Self::Error>
            {
                self.0.for_command(fragments, redirects)
            }

            fn case_command(&mut self,
                            fragments: CaseFragments<Self::Command, Self::Word>,
                            redirects: Vec<Self::Redirect>)
                -> Result<Self::CompoundCommand, Self::Error>
            {
                self.0.case_command(fragments, redirects)
            }

            fn compound_command_into_pipeable(&mut self,
                                              cmd: Self::CompoundCommand)
                -> Result<Self::PipeableCommand, Self::Error>
            {
                self.0.compound_command_into_pipeable(cmd)
            }

            fn function_declaration(&mut self,
                                    name: String,
                                    post_name_comments: Vec<Newline>,
                                    body: Self::CompoundCommand)
                -> Result<Self::PipeableCommand, Self::Error>
            {
                self.0.function_declaration(name, post_name_comments, body)
            }

            fn comments(&mut self,
                        comments: Vec<Newline>)
                -> Result<(), Self::Error>
            {
                self.0.comments(comments)
            }

            fn word_fragment(
                &mut self,
                kind: WordKind<Self::Command, Self::WordFragment>,
            ) -> Result<Self::WordFragment, Self::Error> {
                self.0.word_fragment(kind)
            }

            fn word(&mut self,
                    kind: ConcatWordKind<Self::WordFragment>)
                -> Result<Self::Word, Self::Error>
            {
                self.0.word(kind)
            }

            fn redirect(&mut self,
                        kind: RedirectKind<Self::Word>)
                -> Result<Self::Redirect, Self::Error>
            {
                self.0.redirect(kind)
            }
        }
    };
}

type RcCoreBuilder<T, C, W> = AutoconfFullBuilderBase<T, C, W, Rc<ShellCompoundCommand<T, C, W>>>;
type ArcCoreBuilder<T, C, W> = AutoconfFullBuilderBase<T, C, W, Arc<ShellCompoundCommand<T, C, W>>>;

default_builder! {
    /// A `Builder` implementation which builds shell commands
    /// using the (non-atomic) AST definitions in the `ast` module.
    pub struct DefaultBuilder,
    RcCoreBuilder,
    TopLevelWord,
    M4Word,
    TopLevelCommand,
    ShellPipeableCommand,
}

default_builder! {
    /// A `Builder` implementation which builds shell commands
    /// using the (atomic) AST definitions in the `ast` module.
    pub struct AtomicDefaultBuilder,
    ArcCoreBuilder,
    AtomicTopLevelWord,
    M4Word,
    AtomicTopLevelCommand,
    AtomicShellPipeableCommand,
}

/// A `DefaultBuilder` implementation which uses regular `String`s when
/// representing shell words.
pub type StringBuilder = DefaultBuilder<String>;

/// A `DefaultBuilder` implementation which uses `Rc<String>`s when
/// representing shell words.
pub type RcBuilder = DefaultBuilder<Rc<String>>;

/// A `DefaultBuilder` implementation which uses `Arc<String>`s when
/// representing shell words.
pub type ArcBuilder = AtomicDefaultBuilder<Arc<String>>;

/// The actual provided `Builder` implementation.
/// The various type parameters are used to swap out atomic/non-atomic AST versions.
pub struct AutoconfFullBuilderBase<T, C, W, F> {
    phantom_data: PhantomData<(T, C, W, F)>,
}

impl<T, C, W, F> fmt::Debug for AutoconfFullBuilderBase<T, C, W, F> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.debug_struct("CoreBuilder").finish()
    }
}

impl<T, C, W, F> Clone for AutoconfFullBuilderBase<T, C, W, F> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T, C, W, F> Copy for AutoconfFullBuilderBase<T, C, W, F> {}

impl<T, C, W, F> Default for AutoconfFullBuilderBase<T, C, W, F> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T, C, W, F> AutoconfFullBuilderBase<T, C, W, F> {
    /// Constructs a builder.
    pub fn new() -> Self {
        AutoconfFullBuilderBase {
            phantom_data: PhantomData,
        }
    }
}

type BuilderPipeableCommand<T, C, W, F> = PipeableCommand<
    T,
    Box<SimpleCommand<T, W, Redirect<W>>>,
    Box<ShellCompoundCommand<T, C, W>>,
    F,
>;

impl<L, C, W, F> BuilderBase for AutoconfFullBuilderBase<L, C, W, F> {
    type Command = C;
    type CompoundCommand = ShellCompoundCommand<L, Self::Command, Self::Word>;
    type Word = W;
    type WordFragment = M4Word<L, Self::Command, Self::Word>;
    type Redirect = Redirect<Self::Word>;
    type Error = Void;
}

impl<L, C, W, F> M4Builder for AutoconfFullBuilderBase<L, C, W, F>
where
    L: From<String>,
    C: From<Command<AndOrList<ListableCommand<BuilderPipeableCommand<L, C, W, F>>>>>,
    W: From<ShellWord<L, C, W>>,
    F: From<ShellCompoundCommand<L, C, W>>,
{
    type M4Macro = M4Macro<Self::Command, Self::Word>;
    type M4Argument = M4Argument<Self::Command, Self::Word>;
    fn macro_into_compound_command(
        &mut self,
        macro_call: M4Macro<Self::Command, Self::Word>,
        redirects: Vec<Self::Redirect>,
    ) -> Result<Self::CompoundCommand, Self::Error> {
        Ok(CompoundCommand {
            kind: CompoundCommandKind::Macro(macro_call),
            io: redirects,
        })
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

impl<L, C, W, F> ShellBuilder for AutoconfFullBuilderBase<L, C, W, F>
where
    L: Into<String> + From<String>,
    C: From<Command<AndOrList<ListableCommand<BuilderPipeableCommand<L, C, W, F>>>>>,
    W: From<ShellWord<L, C, W>>,
    F: From<ShellCompoundCommand<L, C, W>>,
{
    type CommandList = AndOrList<Self::ListableCommand>;
    type ListableCommand = ListableCommand<Self::PipeableCommand>;
    type PipeableCommand = BuilderPipeableCommand<L, C, W, F>;

    /// Constructs a `Command::Job` node with the provided inputs if the command
    /// was delimited by an ampersand or the command itself otherwise.
    fn complete_command(
        &mut self,
        _pre_cmd_comments: Vec<Newline>,
        list: Self::CommandList,
        separator: SeparatorKind,
        _cmd_comment: Option<Newline>,
        _range: (usize, usize),
    ) -> Result<Self::Command, Self::Error> {
        let cmd = match separator {
            SeparatorKind::Semi | SeparatorKind::Other | SeparatorKind::Newline => {
                Command::List(list)
            }
            SeparatorKind::Amp => Command::Job(list),
        };

        Ok(cmd.into())
    }

    /// Constructs a `Command::List` node with the provided inputs.
    fn and_or_list(
        &mut self,
        first: Self::ListableCommand,
        rest: Vec<(Vec<Newline>, AndOr<Self::ListableCommand>)>,
    ) -> Result<Self::CommandList, Self::Error> {
        Ok(AndOrList {
            first,
            rest: rest.into_iter().map(|(_, c)| c).collect(),
        })
    }

    /// Constructs a `Command::Pipe` node with the provided inputs or a `Command::Simple`
    /// node if only a single command with no status inversion is supplied.
    fn pipeline(
        &mut self,
        bang: bool,
        cmds: Vec<(Vec<Newline>, Self::PipeableCommand)>,
    ) -> Result<Self::ListableCommand, Self::Error> {
        debug_assert!(!cmds.is_empty());
        let mut cmds: Vec<_> = cmds.into_iter().map(|(_, c)| c).collect();

        // Pipe is the only AST node which allows for a status
        // negation, so we are forced to use it even if we have a single
        // command. Otherwise there is no need to wrap it further.
        if bang || cmds.len() > 1 {
            cmds.shrink_to_fit();
            Ok(ListableCommand::Pipe(bang, cmds))
        } else {
            Ok(ListableCommand::Single(cmds.pop().unwrap()))
        }
    }

    /// Constructs a `Command::Simple` node with the provided inputs.
    fn simple_command(
        &mut self,
        redirects_or_env_vars: Vec<RedirectOrEnvVar<Self::Redirect, String, Self::Word>>,
        mut redirects_or_cmd_words: Vec<RedirectOrCmdWord<Self::Redirect, Self::Word>>,
    ) -> Result<Self::PipeableCommand, Self::Error> {
        let redirects_or_env_vars = redirects_or_env_vars
            .into_iter()
            .map(|roev| match roev {
                RedirectOrEnvVar::Redirect(red) => RedirectOrEnvVar::Redirect(red),
                RedirectOrEnvVar::EnvVar(k, v) => RedirectOrEnvVar::EnvVar(k.into(), v),
            })
            .collect();

        redirects_or_cmd_words.shrink_to_fit();

        Ok(PipeableCommand::Simple(Box::new(SimpleCommand {
            redirects_or_env_vars,
            redirects_or_cmd_words,
        })))
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
        Ok(CompoundCommand {
            kind: CompoundCommandKind::Brace(cmds),
            io: redirects,
        })
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
        Ok(CompoundCommand {
            kind: CompoundCommandKind::Subshell(cmds),
            io: redirects,
        })
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

        let guard_body_pair = GuardBodyPair { guard, body };

        let loop_cmd = match kind {
            LoopKind::While => CompoundCommandKind::While(guard_body_pair),
            LoopKind::Until => CompoundCommandKind::Until(guard_body_pair),
        };

        Ok(CompoundCommand {
            kind: loop_cmd,
            io: redirects,
        })
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

                GuardBodyPair { guard, body }
            })
            .collect();

        let else_branch = else_branch.map(
            |CommandGroup {
                 commands: mut els, ..
             }| {
                els.shrink_to_fit();
                els
            },
        );

        redirects.shrink_to_fit();

        Ok(CompoundCommand {
            kind: CompoundCommandKind::If {
                conditionals,
                else_branch,
            },
            io: redirects,
        })
    }

    /// Constructs a `CompoundCommand::For` node with the provided inputs.
    fn for_command(
        &mut self,
        fragments: ForFragments<Self::Command, Self::Word>,
        mut redirects: Vec<Self::Redirect>,
    ) -> Result<Self::CompoundCommand, Self::Error> {
        let words = fragments.words.map(|(_, mut words, _)| {
            words.shrink_to_fit();
            words
        });

        let mut body = fragments.body.commands;
        body.shrink_to_fit();
        redirects.shrink_to_fit();

        Ok(CompoundCommand {
            kind: CompoundCommandKind::For {
                var: fragments.var.into(),
                words,
                body,
            },
            io: redirects,
        })
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

                PatternBodyPair { comments, patterns, body }
            })
            .collect();

        redirects.shrink_to_fit();
        Ok(CompoundCommand {
            kind: CompoundCommandKind::Case {
                word: fragments.word,
                arms,
            },
            io: redirects,
        })
    }

    /// Converts a `CompoundCommand` into a `PipeableCommand`.
    fn compound_command_into_pipeable(
        &mut self,
        cmd: Self::CompoundCommand,
    ) -> Result<Self::PipeableCommand, Self::Error> {
        Ok(PipeableCommand::Compound(Box::new(cmd)))
    }

    /// Constructs a `Command::FunctionDef` node with the provided inputs.
    fn function_declaration(
        &mut self,
        name: String,
        _post_name_comments: Vec<Newline>,
        body: Self::CompoundCommand,
    ) -> Result<Self::PipeableCommand, Self::Error> {
        Ok(PipeableCommand::FunctionDef(name.into(), body.into()))
    }

    /// Ignored by the builder.
    fn comments(&mut self, _comments: Vec<Newline>) -> Result<(), Self::Error> {
        Ok(())
    }

    fn word_fragment(
        &mut self,
        kind: WordKind<Self::Command, Self::WordFragment>,
    ) -> Result<Self::WordFragment, Self::Error> {
        use crate::ast::builder::ParameterSubstitutionKind::*;
        macro_rules! map {
            ($pat:expr) => {
                match $pat {
                    Some(w) => Some(self.word(w)?),
                    None => None,
                }
            };
        }
        let simple = match kind {
            WordKind::Literal(s) => SimpleWord::Literal(s.into()),
            WordKind::Escaped(s) => SimpleWord::Escaped(s.into()),
            WordKind::Param(p) => SimpleWord::Param(map_param(p)),
            WordKind::Star => SimpleWord::Star,
            WordKind::Question => SimpleWord::Question,
            WordKind::SquareOpen => SimpleWord::SquareOpen,
            WordKind::SquareClose => SimpleWord::SquareClose,
            WordKind::Tilde => SimpleWord::Tilde,
            WordKind::Colon => SimpleWord::Colon,

            WordKind::CommandSubst(c) => {
                SimpleWord::Subst(Box::new(ParameterSubstitution::Command(c.commands)))
            }

            WordKind::Subst(s) => {
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
                SimpleWord::Subst(Box::new(subst))
            }
        };

        Ok(MayM4::Shell(simple))
    }

    /// Constructs a `ast::Word` from the provided input.
    /// TODO: Break this methods into 4 or some smaller methods
    /// 1. fn arithmetic -> Arithmetic (can be implemented at the trait level)
    /// 2. fn param -> B::Param
    /// 3. fn m4_word -> B::WordFragment
    fn word(
        &mut self,
        kind: ConcatWordKind<Self::WordFragment>,
    ) -> Result<Self::Word, Self::Error> {
        let map_quote_word = |kind| {
            let word = match kind {
                QuoteWordKind::Simple(s) => Word::Simple(s),
                QuoteWordKind::SingleQuoted(s) => Word::SingleQuoted(s.into()),
                QuoteWordKind::DoubleQuoted(v) => Word::DoubleQuoted(v),
            };
            Ok(word)
        };

        let word = match compress(kind) {
            ConcatWordKind::Single(s) => ComplexWord::Single(map_quote_word(s)?),
            ConcatWordKind::Concat(words) => ComplexWord::Concat(
                words
                    .into_iter()
                    .map(map_quote_word)
                    .collect::<Result<Vec<_>, _>>()?,
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

fn compress<L, C, W>(word: ConcatWordKind<M4Word<L, C, W>>) -> ConcatWordKind<M4Word<L, C, W>>
where
    L: Into<String> + From<String>,
{
    use crate::ast::builder::ConcatWordKind::*;
    use crate::ast::builder::QuoteWordKind::*;
    use crate::ast::MayM4::*;
    use crate::ast::SimpleWord::*;

    fn coalesce_m4_word<L, C, W>(
        a: M4Word<L, C, W>,
        b: M4Word<L, C, W>,
    ) -> CoalesceResult<M4Word<L, C, W>>
    where
        L: Into<String> + From<String>,
    {
        match (a, b) {
            (Shell(Literal(a)), Shell(Literal(b))) => {
                let (mut a, b) = (a.into(), b.into());
                a.push_str(&b);
                Ok(Shell(Literal(a.into())))
            }
            (a, b) => Err((a, b)),
        }
    }

    fn coalesce_quote_word<L, C, W>(
        a: QuoteWordKind<M4Word<L, C, W>>,
        b: QuoteWordKind<M4Word<L, C, W>>,
    ) -> CoalesceResult<QuoteWordKind<M4Word<L, C, W>>>
    where
        L: Into<String> + From<String>,
    {
        match (a, b) {
            (Simple(a), Simple(b)) => coalesce_m4_word(a, b)
                .map(|s| Simple(s))
                .map_err(|(a, b)| (Simple(a), Simple(b))),
            (SingleQuoted(mut a), SingleQuoted(b)) => {
                a.push_str(&b);
                Ok(SingleQuoted(a))
            }
            (DoubleQuoted(a), DoubleQuoted(b)) => {
                let quoted =
                    Coalesce::new(a.into_iter().chain(b), |a, b| coalesce_m4_word(a, b)).collect();
                Ok(DoubleQuoted(quoted))
            }
            (a, b) => Err((a, b)),
        }
    }

    match word {
        Single(s) => Single(match s {
            s @ Simple(_) | s @ SingleQuoted(_) => s,
            DoubleQuoted(v) => DoubleQuoted(Coalesce::new(v, coalesce_m4_word).collect()),
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
