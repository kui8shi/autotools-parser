use autotools_parser::ast::minimal::AcWord;
use autotools_parser::ast::minimal::CompoundCommand::*;
use autotools_parser::ast::minimal::Condition;
use autotools_parser::ast::minimal::GuardBodyPair;
use autotools_parser::ast::minimal::Operator;
use autotools_parser::ast::minimal::Word;
use autotools_parser::ast::minimal::WordFragment::*;
use autotools_parser::ast::minimal::{AcCommand, WordFragment};
use autotools_parser::ast::MayM4::{self, *};
use autotools_parser::ast::PatternBodyPair;
use autotools_parser::ast::{Arithmetic, Parameter, ParameterSubstitution};
use autotools_parser::m4_macro::M4Argument;
use autotools_parser::m4_macro::M4Macro;
use autotools_parser::parse::SourcePos;

pub type MinimalWord = AcWord<String>;
pub type MinimalWordFragment = WordFragment<String, MinimalCommand, MinimalWord>;
pub type MinimalMayM4Word = MayM4<MinimalWordFragment, MinimalM4Macro>;
pub type MinimalCommand = AcCommand<String, AcWord<String>>;
pub type MinimalParameterSubstitution =
    ParameterSubstitution<Parameter<String>, MinimalCommand, MinimalWord, Arithmetic<String>>;
pub type MinimalOperator = Operator<MinimalWord>;
pub type MinimalCondition = Condition<MinimalCommand, MinimalWord>;
pub type MinimalM4Argument = M4Argument<MinimalCommand, MinimalWord>;
pub type MinimalM4Macro = M4Macro<MinimalCommand, MinimalWord>;

pub fn lit(s: &str) -> MinimalWordFragment {
    Literal(String::from(s))
}

pub fn may_lit(s: &str) -> MinimalMayM4Word {
    Shell(lit(s))
}

pub fn word_lit(s: &str) -> MinimalWord {
    word(may_lit(s))
}

pub fn escaped(s: &str) -> MinimalWordFragment {
    Escaped(String::from(s))
}

pub fn subst(s: MinimalParameterSubstitution) -> MinimalWordFragment {
    Subst(Box::new(s))
}

pub fn double_quoted(v: &[WordFragment<String, MinimalCommand, MinimalWord>]) -> MinimalMayM4Word {
    Shell(DoubleQuoted(v.to_owned()))
}

pub fn param(p: Parameter<String>) -> MinimalWordFragment {
    Param(p)
}

pub fn var(v: &str) -> MinimalWordFragment {
    Param(Parameter::Var(v.to_owned()))
}

pub fn word_var(v: &str) -> MinimalWord {
    word(Shell(Param(Parameter::Var(v.to_owned()))))
}

pub fn word(fragment: MinimalMayM4Word) -> MinimalWord {
    Word::Single(fragment).into()
}

pub fn words(fragments: &[MinimalMayM4Word]) -> MinimalWord {
    Word::Concat(fragments.to_owned()).into()
}

pub fn assign(name: &str, val: MinimalWord) -> MinimalCommand {
    AcCommand::new_assign(name.into(), val)
}

pub fn cmd_from_lits(cmd: &str, args: &[&str]) -> MinimalCommand {
    let mut cmd_args = Vec::with_capacity(args.len() + 1);
    cmd_args.push(word(Shell(lit(cmd))));
    cmd_args.extend(args.iter().map(|&a| word(Shell(lit(a)))));

    AcCommand::new_cmd(cmd_args)
}

pub fn cmd_from_words(cmd: &str, args: &[MinimalWord]) -> MinimalCommand {
    let mut cmd_args = Vec::with_capacity(args.len() + 1);
    cmd_args.push(word(Shell(lit(cmd))));
    cmd_args.extend(args.to_vec());

    AcCommand::new_cmd(cmd_args)
}

pub fn empty_cmd() -> MinimalCommand {
    AcCommand::new_cmd(vec![])
}

pub fn cmd_if(cond: MinimalCondition, cmds: &[MinimalCommand]) -> MinimalCommand {
    AcCommand::new_compound(If {
        conditionals: vec![GuardBodyPair {
            condition: cond,
            body: cmds.to_vec(),
        }],
        else_branch: vec![],
    })
}

pub fn cmd_case(
    target: MinimalWord,
    patterns: &[(&[MinimalWord], &[MinimalCommand])],
) -> MinimalCommand {
    AcCommand::new_compound(Case {
        word: target,
        arms: patterns
            .iter()
            .map(|(p, c)| PatternBodyPair {
                patterns: p.to_vec(),
                body: c.to_vec(),
            })
            .collect(),
    })
}

pub fn cmd_brace(cmds: &[MinimalCommand]) -> MinimalCommand {
    AcCommand::new_compound(Brace(cmds.to_owned()))
}

pub fn cmd_macro(m: MinimalM4Macro) -> MinimalCommand {
    AcCommand::new_macro(m)
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
    M4Argument::Word(Word::Single(Shell(var(s))).into())
}

pub fn m4_word(w: MinimalMayM4Word) -> MinimalM4Argument {
    M4Argument::Word(Word::Single(w).into())
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

pub fn m4_macro_as_word(name: &str, args: &[MinimalM4Argument]) -> MinimalMayM4Word {
    Macro(m4_macro(name, args))
}

pub fn src(byte: usize, line: usize, col: usize) -> SourcePos {
    SourcePos { byte, line, col }
}
