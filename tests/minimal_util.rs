use autoconf_parser::ast::minimal::Command::*;
use autoconf_parser::ast::minimal::Condition;
use autoconf_parser::ast::minimal::GuardBodyPair;
use autoconf_parser::ast::minimal::Operator;
use autoconf_parser::ast::minimal::Word;
use autoconf_parser::ast::minimal::WordFragment::*;
use autoconf_parser::ast::minimal::{CommandWrapper, WordFragment};
use autoconf_parser::ast::minimal::{CompoundCommand, CompoundCommand::*};
use autoconf_parser::ast::PatternBodyPair;
use autoconf_parser::ast::{Arithmetic, Parameter, ParameterSubstitution};
use autoconf_parser::m4_macro::M4Argument;
use autoconf_parser::m4_macro::M4Macro;
use autoconf_parser::parse::*;

pub type MinimalWord = Word<String, MinimalCommand>;
pub type MinimalWordFragment = WordFragment<String, MinimalWord, MinimalCommand>;
pub type MinimalCommand = CommandWrapper<String>;
pub type MinimalParameterSubstitution =
    ParameterSubstitution<Parameter<String>, MinimalWord, MinimalCommand, Arithmetic<String>>;
pub type MinimalOperator = Operator<MinimalWord>;
pub type MinimalCondition = Condition<MinimalWord, MinimalCommand>;
pub type MinimalM4Argument = M4Argument<MinimalWord, MinimalCommand>;
pub type MinimalM4Macro = M4Macro<MinimalWord, MinimalCommand>;

pub fn lit(s: &str) -> MinimalWordFragment {
    Literal(String::from(s))
}

pub fn escaped(s: &str) -> MinimalWordFragment {
    Escaped(String::from(s))
}

pub fn subst(s: MinimalParameterSubstitution) -> MinimalWordFragment {
    Subst(Box::new(s))
}

pub fn double_quoted(v: &[MinimalWordFragment]) -> MinimalWordFragment {
    DoubleQuoted(v.to_owned())
}

pub fn param(p: Parameter<String>) -> MinimalWordFragment {
    Param(p)
}

pub fn var(v: &str) -> MinimalWordFragment {
    Param(Parameter::Var(v.to_owned()))
}

pub fn word(fragment: MinimalWordFragment) -> MinimalWord {
    Word::Single(fragment)
}

pub fn words(fragments: &[MinimalWordFragment]) -> MinimalWord {
    Word::Concat(fragments.to_owned())
}

pub fn assign(name: &str, val: MinimalWord) -> MinimalCommand {
    CommandWrapper::new(Assignment(name.into(), val))
}

pub fn cmd_from_lits(cmd: &str, args: &[&str]) -> MinimalCommand {
    let mut cmd_args = Vec::with_capacity(args.len() + 1);
    cmd_args.push(word(lit(cmd)));
    cmd_args.extend(args.iter().map(|&a| word(lit(a))));

    CommandWrapper::new(Cmd(cmd_args))
}

pub fn cmd_from_words(cmd: &str, args: &[MinimalWord]) -> MinimalCommand {
    let mut cmd_args = Vec::with_capacity(args.len() + 1);
    cmd_args.push(word(lit(cmd)));
    cmd_args.extend(args.to_vec());

    CommandWrapper::new(Cmd(cmd_args))
}

pub fn empty_cmd() -> MinimalCommand {
    CommandWrapper::new(Cmd(vec![]))
}

pub fn cmd_if(cond: MinimalCondition, cmds: &[MinimalCommand]) -> MinimalCommand {
    CommandWrapper::new(Compound(If {
        conditionals: vec![GuardBodyPair {
            condition: cond,
            body: cmds.to_vec(),
        }],
        else_branch: vec![],
    }))
}

pub fn cmd_case(
    target: MinimalWord,
    patterns: &[(&[MinimalWord], &[MinimalCommand])],
) -> MinimalCommand {
    CommandWrapper::new(Compound(Case {
        word: target,
        arms: patterns
            .iter()
            .map(|(p, c)| PatternBodyPair {
                patterns: p.to_vec(),
                body: c.to_vec(),
            })
            .collect(),
    }))
}

pub fn cmd_brace(cmds: &[MinimalCommand]) -> MinimalCommand {
    CommandWrapper::new(Compound(Brace(cmds.to_owned())))
}

pub fn cmd_macro(m: MinimalM4Macro) -> MinimalCommand {
    CommandWrapper::new(Compound(CompoundCommand::Macro(m)))
}

pub fn eq(lhs: MinimalWord, rhs: MinimalWord) -> MinimalOperator {
    Operator::Eq(lhs, rhs)
}
pub fn neq(lhs: MinimalWord, rhs: MinimalWord) -> MinimalOperator {
    Operator::Neq(lhs, rhs)
}
pub fn ge(lhs: MinimalWord, rhs: MinimalWord) -> MinimalOperator {
    Operator::Ge(lhs, rhs)
}
pub fn gt(lhs: MinimalWord, rhs: MinimalWord) -> MinimalOperator {
    Operator::Gt(lhs, rhs)
}
pub fn le(lhs: MinimalWord, rhs: MinimalWord) -> MinimalOperator {
    Operator::Le(lhs, rhs)
}
pub fn lt(lhs: MinimalWord, rhs: MinimalWord) -> MinimalOperator {
    Operator::Lt(lhs, rhs)
}
pub fn empty(rhs: MinimalWord) -> MinimalOperator {
    Operator::Empty(rhs)
}
pub fn nonempty(rhs: MinimalWord) -> MinimalOperator {
    Operator::NonEmpty(rhs)
}
pub fn dir(rhs: MinimalWord) -> MinimalOperator {
    Operator::Dir(rhs)
}
pub fn file(rhs: MinimalWord) -> MinimalOperator {
    Operator::File(rhs)
}
pub fn cond(op: MinimalOperator) -> MinimalCondition {
    Condition::Cond(op)
}
pub fn cond_and(cond1: MinimalCondition, cond2: MinimalCondition) -> MinimalCondition {
    Condition::And(Box::new(cond1), Box::new(cond2))
}
pub fn cond_or(cond1: MinimalCondition, cond2: MinimalCondition) -> MinimalCondition {
    Condition::Or(Box::new(cond1), Box::new(cond2))
}
pub fn cond_eval(cmds: &[MinimalCommand]) -> MinimalCondition {
    Condition::Eval(cmds.to_owned())
}
pub fn cond_return_zero(cmd: MinimalCommand) -> MinimalCondition {
    Condition::ReturnZero(Box::new(cmd))
}

pub fn m4_lit(s: &str) -> MinimalM4Argument {
    M4Argument::Literal(s.to_string())
}

pub fn m4_var(s: &str) -> MinimalM4Argument {
    M4Argument::Word(Word::Single(var(s)))
}

pub fn m4_word(w: MinimalWordFragment) -> MinimalM4Argument {
    M4Argument::Word(Word::Single(w.clone()))
}

pub fn m4_arr(words: &[MinimalWord]) -> MinimalM4Argument {
    M4Argument::Array(words.to_owned())
}

pub fn m4_prog(p: &str) -> MinimalM4Argument {
    M4Argument::Program(p.to_string())
}

pub fn m4_cmd(cmd: MinimalCommand) -> MinimalM4Argument {
    M4Argument::Commands(vec![cmd])
}

pub fn m4_cmds(cmds: &[MinimalCommand]) -> MinimalM4Argument {
    M4Argument::Commands(cmds.to_owned())
}

pub fn m4_raw(unknown: &str) -> MinimalM4Argument {
    M4Argument::Unknown(unknown.to_string())
}

pub fn m4_macro(name: &str, args: &[MinimalM4Argument]) -> MinimalM4Macro {
    MinimalM4Macro::new(name.to_string(), args.to_owned())
}

pub fn m4_macro_as_cmd(name: &str, args: &[MinimalM4Argument]) -> MinimalCommand {
    cmd_macro(m4_macro(name, args))
}

pub fn m4_macro_as_word(name: &str, args: &[MinimalM4Argument]) -> MinimalWordFragment {
    WordFragment::Macro(m4_macro(name, args))
}

pub fn src(byte: usize, line: usize, col: usize) -> SourcePos {
    SourcePos { byte, line, col }
}
