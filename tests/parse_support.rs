// Certain helpers may only be used by specific tests,
// suppress dead_code warnings since the compiler can't
// see our intent
#![allow(dead_code)]

use autoconf_parser::ast::Command::*;
use autoconf_parser::ast::ComplexWord::*;
use autoconf_parser::ast::MayM4::*;
use autoconf_parser::ast::PipeableCommand::*;
use autoconf_parser::ast::SimpleWord::*;
use autoconf_parser::ast::*;
use autoconf_parser::lexer::Lexer;
use autoconf_parser::autoconf::*;
use autoconf_parser::token::Token;

pub fn lit(s: &str) -> DefaultWord {
    Word::Simple(Shell(Literal(String::from(s))))
}

pub fn escaped(s: &str) -> DefaultWord {
    Word::Simple(Shell(Escaped(String::from(s))))
}

pub fn subst(s: DefaultParameterSubstitution) -> DefaultWord {
    Word::Simple(Shell(Subst(Box::new(s))))
}

pub fn single_quoted(s: &str) -> TopLevelWord<String> {
    TopLevelWord(Single(Word::SingleQuoted(String::from(s))))
}

pub fn double_quoted(s: &[&str]) -> TopLevelWord<String> {
    TopLevelWord(Single(Word::DoubleQuoted(
        s.iter().map(|e| Shell(from_str(e))).collect(),
    )))
}

pub fn word(s: &str) -> TopLevelWord<String> {
    TopLevelWord(Single(Word::Simple(Shell(from_str(s)))))
}

pub fn word_escaped(s: &str) -> TopLevelWord<String> {
    TopLevelWord(Single(escaped(s)))
}

pub fn word_subst(s: DefaultParameterSubstitution) -> TopLevelWord<String> {
    TopLevelWord(Single(subst(s)))
}

pub fn word_param(p: DefaultParameter) -> TopLevelWord<String> {
    TopLevelWord(Single(Word::Simple(Shell(Param(p)))))
}

pub fn from_str(s: &str) -> DefaultSimpleWord {
    match s {
        "[" => SquareOpen,
        "]" => SquareClose,
        "~" => Tilde,
        "*" => Star,
        "?" => Question,
        "\\" => Escaped(s.into()),
        ":" => Colon,
        s => {
            if s.starts_with("$")
                && s.len() > 1
                && s[1..].chars().all(|c| c.is_alphanumeric() || c == '_')
            {
                let s = &s[1..];
                if s.len() == 1 && s.chars().last().unwrap().is_numeric() {
                    Param(Parameter::Positional(s.parse::<u32>().unwrap()))
                } else {
                    Param(Parameter::Var(s.into()))
                }
            } else {
                Literal(s.into())
            }
        }
    }
}

pub fn concat_words(s: &[&str]) -> TopLevelWord<String> {
    TopLevelWord(Concat(
        s.iter().map(|s| Word::Simple(Shell(from_str(s)))).collect(),
    ))
}

pub fn make_parser(src: &str) -> DefaultParser<Lexer<std::str::Chars<'_>>> {
    DefaultParser::new(Lexer::new(src.chars()))
}

pub fn make_parser_from_tokens(src: Vec<Token>) -> DefaultParser<std::vec::IntoIter<Token>> {
    DefaultParser::new(src)
}

pub fn cmd_args_words(cmd: &str, args: &[TopLevelWord<String>]) -> Box<DefaultSimpleCommand> {
    let mut cmd_args = Vec::with_capacity(args.len() + 1);
    cmd_args.push(RedirectOrCmdWord::CmdWord(word(cmd)));
    cmd_args.extend(args.iter().map(|a| RedirectOrCmdWord::CmdWord(a.clone())));

    Box::new(SimpleCommand {
        redirects_or_env_vars: vec![],
        redirects_or_cmd_words: cmd_args,
    })
}

pub fn cmd_args_simple(cmd: &str, args: &[&str]) -> Box<DefaultSimpleCommand> {
    let mut cmd_args = Vec::with_capacity(args.len() + 1);
    cmd_args.push(RedirectOrCmdWord::CmdWord(word(cmd)));
    cmd_args.extend(args.iter().map(|&a| {
        RedirectOrCmdWord::CmdWord(TopLevelWord(Single(Word::Simple(Shell(from_str(a))))))
    }));

    Box::new(SimpleCommand {
        redirects_or_env_vars: vec![],
        redirects_or_cmd_words: cmd_args,
    })
}

pub fn cmd_simple(cmd: &str) -> Box<DefaultSimpleCommand> {
    cmd_args_simple(cmd, &[])
}

pub fn cmd_args(cmd: &str, args: &[&str]) -> TopLevelCommand<String> {
    TopLevelCommand(List(CommandList {
        first: ListableCommand::Single(Simple(cmd_args_simple(cmd, args))),
        rest: vec![],
    }))
}

pub fn cmd(cmd: &str) -> TopLevelCommand<String> {
    cmd_args(cmd, &[])
}

pub fn cmd_from_simple(cmd: DefaultSimpleCommand) -> TopLevelCommand<String> {
    TopLevelCommand(List(CommandList {
        first: ListableCommand::Single(Simple(Box::new(cmd))),
        rest: vec![],
    }))
}

pub fn src(byte: usize, line: usize, col: usize) -> SourcePos {
    SourcePos { byte, line, col }
}
