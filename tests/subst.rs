#![deny(rust_2018_idioms)]
use autotools_parser::ast::Parameter::*;
use autotools_parser::ast::ParameterSubstitution::*;
use autotools_parser::parse::ParseErrorKind::*;
use autotools_parser::token::Token;

mod minimal_util;
use crate::minimal_util::*;

#[test]
fn test_parameter_substitution() {
    let words = vec![
        Len(At),
        Len(Star),
        Len(Pound),
        Len(Question),
        Len(Dash),
        Len(Dollar),
        Len(Bang),
        Len(Var(String::from("foo"))),
        Len(Positional(3)),
        Len(Positional(1000)),
        Command(vec![cmd_from_lits("echo", &["foo"])]),
    ];

    let mut p = make_parser("${#@}${#*}${##}${#?}${#-}${#$}${#!}${#foo}${#3}${#1000}$(echo foo)");
    for param in words {
        let correct = word(subst(param));
        assert_eq!(correct, p.parameter().unwrap());
    }

    assert_eq!(Err(UnexpectedEOF.into()), p.parameter()); // Stream should be exhausted
}

#[test]
fn test_parameter_substitution_smallest_suffix() {
    let w = word_lit("foo");

    let substs = vec![
        RemoveSmallestSuffix(At, Some(w.clone())),
        RemoveSmallestSuffix(Star, Some(w.clone())),
        RemoveSmallestSuffix(Pound, Some(w.clone())),
        RemoveSmallestSuffix(Question, Some(w.clone())),
        RemoveSmallestSuffix(Dash, Some(w.clone())),
        RemoveSmallestSuffix(Dollar, Some(w.clone())),
        RemoveSmallestSuffix(Bang, Some(w.clone())),
        RemoveSmallestSuffix(Positional(0), Some(w.clone())),
        RemoveSmallestSuffix(Positional(10), Some(w.clone())),
        RemoveSmallestSuffix(Positional(100), Some(w.clone())),
        RemoveSmallestSuffix(Var(String::from("foo_bar123")), Some(w)),
        RemoveSmallestSuffix(At, None),
        RemoveSmallestSuffix(Star, None),
        RemoveSmallestSuffix(Pound, None),
        RemoveSmallestSuffix(Question, None),
        RemoveSmallestSuffix(Dash, None),
        RemoveSmallestSuffix(Dollar, None),
        RemoveSmallestSuffix(Bang, None),
        RemoveSmallestSuffix(Positional(0), None),
        RemoveSmallestSuffix(Positional(10), None),
        RemoveSmallestSuffix(Positional(100), None),
        RemoveSmallestSuffix(Var(String::from("foo_bar123")), None),
    ];

    let src = "${@%foo}${*%foo}${#%foo}${?%foo}${-%foo}${$%foo}${!%foo}${0%foo}${10%foo}${100%foo}${foo_bar123%foo}${@%}${*%}${#%}${?%}${-%}${$%}${!%}${0%}${10%}${100%}${foo_bar123%}";
    let mut p = make_parser(src);

    for s in substs {
        assert_eq!(word(subst(s)), p.parameter().unwrap());
    }

    assert_eq!(Err(UnexpectedEOF.into()), p.parameter()); // Stream should be exhausted
}

#[test]
fn test_parameter_substitution_largest_suffix() {
    let w = word_lit("foo");

    let substs = vec![
        RemoveLargestSuffix(At, Some(w.clone())),
        RemoveLargestSuffix(Star, Some(w.clone())),
        RemoveLargestSuffix(Pound, Some(w.clone())),
        RemoveLargestSuffix(Question, Some(w.clone())),
        RemoveLargestSuffix(Dash, Some(w.clone())),
        RemoveLargestSuffix(Dollar, Some(w.clone())),
        RemoveLargestSuffix(Bang, Some(w.clone())),
        RemoveLargestSuffix(Positional(0), Some(w.clone())),
        RemoveLargestSuffix(Positional(10), Some(w.clone())),
        RemoveLargestSuffix(Positional(100), Some(w.clone())),
        RemoveLargestSuffix(Var(String::from("foo_bar123")), Some(w)),
        RemoveLargestSuffix(At, None),
        RemoveLargestSuffix(Star, None),
        RemoveLargestSuffix(Pound, None),
        RemoveLargestSuffix(Question, None),
        RemoveLargestSuffix(Dash, None),
        RemoveLargestSuffix(Dollar, None),
        RemoveLargestSuffix(Bang, None),
        RemoveLargestSuffix(Positional(0), None),
        RemoveLargestSuffix(Positional(10), None),
        RemoveLargestSuffix(Positional(100), None),
        RemoveLargestSuffix(Var(String::from("foo_bar123")), None),
    ];

    let src = "${@%%foo}${*%%foo}${#%%foo}${?%%foo}${-%%foo}${$%%foo}${!%%foo}${0%%foo}${10%%foo}${100%%foo}${foo_bar123%%foo}${@%%}${*%%}${#%%}${?%%}${-%%}${$%%}${!%%}${0%%}${10%%}${100%%}${foo_bar123%%}";
    let mut p = make_parser(src);

    for s in substs {
        assert_eq!(word(subst(s)), p.parameter().unwrap());
    }

    assert_eq!(Err(UnexpectedEOF.into()), p.parameter()); // Stream should be exhausted
}

#[test]
fn test_parameter_substitution_smallest_prefix() {
    let w = word_lit("foo");

    let substs = vec![
        RemoveSmallestPrefix(At, Some(w.clone())),
        RemoveSmallestPrefix(Star, Some(w.clone())),
        RemoveSmallestPrefix(Pound, Some(w.clone())),
        RemoveSmallestPrefix(Question, Some(w.clone())),
        RemoveSmallestPrefix(Dash, Some(w.clone())),
        RemoveSmallestPrefix(Dollar, Some(w.clone())),
        RemoveSmallestPrefix(Bang, Some(w.clone())),
        RemoveSmallestPrefix(Positional(0), Some(w.clone())),
        RemoveSmallestPrefix(Positional(10), Some(w.clone())),
        RemoveSmallestPrefix(Positional(100), Some(w.clone())),
        RemoveSmallestPrefix(Var(String::from("foo_bar123")), Some(w)),
        RemoveSmallestPrefix(At, None),
        RemoveSmallestPrefix(Star, None),
        //RemoveSmallestPrefix(Pound, None), // ${##} should parse as Len(#)
        RemoveSmallestPrefix(Question, None),
        RemoveSmallestPrefix(Dash, None),
        RemoveSmallestPrefix(Dollar, None),
        RemoveSmallestPrefix(Bang, None),
        RemoveSmallestPrefix(Positional(0), None),
        RemoveSmallestPrefix(Positional(10), None),
        RemoveSmallestPrefix(Positional(100), None),
        RemoveSmallestPrefix(Var(String::from("foo_bar123")), None),
    ];

    let src = "${@#foo}${*#foo}${##foo}${?#foo}${-#foo}${$#foo}${!#foo}${0#foo}${10#foo}${100#foo}${foo_bar123#foo}${@#}${*#}${?#}${-#}${$#}${!#}${0#}${10#}${100#}${foo_bar123#}";
    let mut p = make_parser(src);

    for s in substs {
        assert_eq!(word(subst(s)), p.parameter().unwrap());
    }

    assert_eq!(Err(UnexpectedEOF.into()), p.parameter()); // Stream should be exhausted
}

#[test]
fn test_parameter_substitution_largest_prefix() {
    let w = word_lit("foo");

    let substs = vec![
        RemoveLargestPrefix(At, Some(w.clone())),
        RemoveLargestPrefix(Star, Some(w.clone())),
        RemoveLargestPrefix(Pound, Some(w.clone())),
        RemoveLargestPrefix(Question, Some(w.clone())),
        RemoveLargestPrefix(Dash, Some(w.clone())),
        RemoveLargestPrefix(Dollar, Some(w.clone())),
        RemoveLargestPrefix(Bang, Some(w.clone())),
        RemoveLargestPrefix(Positional(0), Some(w.clone())),
        RemoveLargestPrefix(Positional(10), Some(w.clone())),
        RemoveLargestPrefix(Positional(100), Some(w.clone())),
        RemoveLargestPrefix(Var(String::from("foo_bar123")), Some(w)),
        RemoveLargestPrefix(At, None),
        RemoveLargestPrefix(Star, None),
        RemoveLargestPrefix(Pound, None),
        RemoveLargestPrefix(Question, None),
        RemoveLargestPrefix(Dash, None),
        RemoveLargestPrefix(Dollar, None),
        RemoveLargestPrefix(Bang, None),
        RemoveLargestPrefix(Positional(0), None),
        RemoveLargestPrefix(Positional(10), None),
        RemoveLargestPrefix(Positional(100), None),
        RemoveLargestPrefix(Var(String::from("foo_bar123")), None),
    ];

    let src = "${@##foo}${*##foo}${###foo}${?##foo}${-##foo}${$##foo}${!##foo}${0##foo}${10##foo}${100##foo}${foo_bar123##foo}${@##}${*##}${###}${?##}${-##}${$##}${!##}${0##}${10##}${100##}${foo_bar123##}";
    let mut p = make_parser(src);

    for s in substs {
        assert_eq!(word(subst(s)), p.parameter().unwrap());
    }

    assert_eq!(Err(UnexpectedEOF.into()), p.parameter()); // Stream should be exhausted
}

#[test]
fn test_parameter_substitution_default() {
    let w = word_lit("foo");

    let substs = vec![
        Default(true, At, Some(w.clone())),
        Default(true, Star, Some(w.clone())),
        Default(true, Pound, Some(w.clone())),
        Default(true, Question, Some(w.clone())),
        Default(true, Dash, Some(w.clone())),
        Default(true, Dollar, Some(w.clone())),
        Default(true, Bang, Some(w.clone())),
        Default(true, Positional(0), Some(w.clone())),
        Default(true, Positional(10), Some(w.clone())),
        Default(true, Positional(100), Some(w.clone())),
        Default(true, Var(String::from("foo_bar123")), Some(w.clone())),
        Default(true, At, None),
        Default(true, Star, None),
        Default(true, Pound, None),
        Default(true, Question, None),
        Default(true, Dash, None),
        Default(true, Dollar, None),
        Default(true, Bang, None),
        Default(true, Positional(0), None),
        Default(true, Positional(10), None),
        Default(true, Positional(100), None),
        Default(true, Var(String::from("foo_bar123")), None),
    ];

    let src = "${@:-foo}${*:-foo}${#:-foo}${?:-foo}${-:-foo}${$:-foo}${!:-foo}${0:-foo}${10:-foo}${100:-foo}${foo_bar123:-foo}${@:-}${*:-}${#:-}${?:-}${-:-}${$:-}${!:-}${0:-}${10:-}${100:-}${foo_bar123:-}";
    let mut p = make_parser(src);
    for s in substs {
        assert_eq!(word(subst(s)), p.parameter().unwrap());
    }
    assert_eq!(Err(UnexpectedEOF.into()), p.parameter()); // Stream should be exhausted

    let substs = vec![
        Default(false, At, Some(w.clone())),
        Default(false, Star, Some(w.clone())),
        Default(false, Pound, Some(w.clone())),
        Default(false, Question, Some(w.clone())),
        Default(false, Dash, Some(w.clone())),
        Default(false, Dollar, Some(w.clone())),
        Default(false, Bang, Some(w.clone())),
        Default(false, Positional(0), Some(w.clone())),
        Default(false, Positional(10), Some(w.clone())),
        Default(false, Positional(100), Some(w.clone())),
        Default(false, Var(String::from("foo_bar123")), Some(w)),
        Default(false, At, None),
        Default(false, Star, None),
        //Default(false, Pound, None), // ${#-} should be a length check of the `-` parameter
        Default(false, Question, None),
        Default(false, Dash, None),
        Default(false, Dollar, None),
        Default(false, Bang, None),
        Default(false, Positional(0), None),
        Default(false, Positional(10), None),
        Default(false, Positional(100), None),
        Default(false, Var(String::from("foo_bar123")), None),
    ];

    let src = "${@-foo}${*-foo}${#-foo}${?-foo}${--foo}${$-foo}${!-foo}${0-foo}${10-foo}${100-foo}${foo_bar123-foo}${@-}${*-}${?-}${--}${$-}${!-}${0-}${10-}${100-}${foo_bar123-}";
    let mut p = make_parser(src);
    for s in substs {
        assert_eq!(word(subst(s)), p.parameter().unwrap());
    }
    assert_eq!(Err(UnexpectedEOF.into()), p.parameter()); // Stream should be exhausted
}

#[test]
fn test_parameter_substitution_error() {
    let w = word_lit("foo");

    let substs = vec![
        Error(true, At, Some(w.clone())),
        Error(true, Star, Some(w.clone())),
        Error(true, Pound, Some(w.clone())),
        Error(true, Question, Some(w.clone())),
        Error(true, Dash, Some(w.clone())),
        Error(true, Dollar, Some(w.clone())),
        Error(true, Bang, Some(w.clone())),
        Error(true, Positional(0), Some(w.clone())),
        Error(true, Positional(10), Some(w.clone())),
        Error(true, Positional(100), Some(w.clone())),
        Error(true, Var(String::from("foo_bar123")), Some(w.clone())),
        Error(true, At, None),
        Error(true, Star, None),
        Error(true, Pound, None),
        Error(true, Question, None),
        Error(true, Dash, None),
        Error(true, Dollar, None),
        Error(true, Bang, None),
        Error(true, Positional(0), None),
        Error(true, Positional(10), None),
        Error(true, Positional(100), None),
        Error(true, Var(String::from("foo_bar123")), None),
    ];

    let src = "${@:?foo}${*:?foo}${#:?foo}${?:?foo}${-:?foo}${$:?foo}${!:?foo}${0:?foo}${10:?foo}${100:?foo}${foo_bar123:?foo}${@:?}${*:?}${#:?}${?:?}${-:?}${$:?}${!:?}${0:?}${10:?}${100:?}${foo_bar123:?}";
    let mut p = make_parser(src);
    for s in substs {
        assert_eq!(word(subst(s)), p.parameter().unwrap());
    }
    assert_eq!(Err(UnexpectedEOF.into()), p.parameter()); // Stream should be exhausted

    let substs = vec![
        Error(false, At, Some(w.clone())),
        Error(false, Star, Some(w.clone())),
        Error(false, Pound, Some(w.clone())),
        Error(false, Question, Some(w.clone())),
        Error(false, Dash, Some(w.clone())),
        Error(false, Dollar, Some(w.clone())),
        Error(false, Bang, Some(w.clone())),
        Error(false, Positional(0), Some(w.clone())),
        Error(false, Positional(10), Some(w.clone())),
        Error(false, Positional(100), Some(w.clone())),
        Error(false, Var(String::from("foo_bar123")), Some(w)),
        Error(false, At, None),
        Error(false, Star, None),
        //Error(false, Pound, None), // ${#?} should be a length check of the `?` parameter
        Error(false, Question, None),
        Error(false, Dash, None),
        Error(false, Dollar, None),
        Error(false, Bang, None),
        Error(false, Positional(0), None),
        Error(false, Positional(10), None),
        Error(false, Positional(100), None),
        Error(false, Var(String::from("foo_bar123")), None),
    ];

    let src = "${@?foo}${*?foo}${#?foo}${??foo}${-?foo}${$?foo}${!?foo}${0?foo}${10?foo}${100?foo}${foo_bar123?foo}${@?}${*?}${??}${-?}${$?}${!?}${0?}${10?}${100?}${foo_bar123?}";
    let mut p = make_parser(src);
    for s in substs {
        assert_eq!(word(subst(s)), p.parameter().unwrap());
    }
    assert_eq!(Err(UnexpectedEOF.into()), p.parameter()); // Stream should be exhausted
}

#[test]
fn test_parameter_substitution_assign() {
    let w = word_lit("foo");

    let substs = vec![
        Assign(true, At, Some(w.clone())),
        Assign(true, Star, Some(w.clone())),
        Assign(true, Pound, Some(w.clone())),
        Assign(true, Question, Some(w.clone())),
        Assign(true, Dash, Some(w.clone())),
        Assign(true, Dollar, Some(w.clone())),
        Assign(true, Bang, Some(w.clone())),
        Assign(true, Positional(0), Some(w.clone())),
        Assign(true, Positional(10), Some(w.clone())),
        Assign(true, Positional(100), Some(w.clone())),
        Assign(true, Var(String::from("foo_bar123")), Some(w.clone())),
        Assign(true, At, None),
        Assign(true, Star, None),
        Assign(true, Pound, None),
        Assign(true, Question, None),
        Assign(true, Dash, None),
        Assign(true, Dollar, None),
        Assign(true, Bang, None),
        Assign(true, Positional(0), None),
        Assign(true, Positional(10), None),
        Assign(true, Positional(100), None),
        Assign(true, Var(String::from("foo_bar123")), None),
    ];

    let src = "${@:=foo}${*:=foo}${#:=foo}${?:=foo}${-:=foo}${$:=foo}${!:=foo}${0:=foo}${10:=foo}${100:=foo}${foo_bar123:=foo}${@:=}${*:=}${#:=}${?:=}${-:=}${$:=}${!:=}${0:=}${10:=}${100:=}${foo_bar123:=}";
    let mut p = make_parser(src);
    for s in substs {
        assert_eq!(word(subst(s)), p.parameter().unwrap());
    }
    assert_eq!(Err(UnexpectedEOF.into()), p.parameter()); // Stream should be exhausted

    let substs = vec![
        Assign(false, At, Some(w.clone())),
        Assign(false, Star, Some(w.clone())),
        Assign(false, Pound, Some(w.clone())),
        Assign(false, Question, Some(w.clone())),
        Assign(false, Dash, Some(w.clone())),
        Assign(false, Dollar, Some(w.clone())),
        Assign(false, Bang, Some(w.clone())),
        Assign(false, Positional(0), Some(w.clone())),
        Assign(false, Positional(10), Some(w.clone())),
        Assign(false, Positional(100), Some(w.clone())),
        Assign(false, Var(String::from("foo_bar123")), Some(w)),
        Assign(false, At, None),
        Assign(false, Star, None),
        Assign(false, Pound, None),
        Assign(false, Question, None),
        Assign(false, Dash, None),
        Assign(false, Dollar, None),
        Assign(false, Bang, None),
        Assign(false, Positional(0), None),
        Assign(false, Positional(10), None),
        Assign(false, Positional(100), None),
        Assign(false, Var(String::from("foo_bar123")), None),
    ];

    let src = "${@=foo}${*=foo}${#=foo}${?=foo}${-=foo}${$=foo}${!=foo}${0=foo}${10=foo}${100=foo}${foo_bar123=foo}${@=}${*=}${#=}${?=}${-=}${$=}${!=}${0=}${10=}${100=}${foo_bar123=}";
    let mut p = make_parser(src);
    for s in substs {
        assert_eq!(word(subst(s)), p.parameter().unwrap());
    }
    assert_eq!(Err(UnexpectedEOF.into()), p.parameter()); // Stream should be exhausted
}

#[test]
fn test_parameter_substitution_alternative() {
    let w = word_lit("foo");

    let substs = vec![
        Alternative(true, At, Some(w.clone())),
        Alternative(true, Star, Some(w.clone())),
        Alternative(true, Pound, Some(w.clone())),
        Alternative(true, Question, Some(w.clone())),
        Alternative(true, Dash, Some(w.clone())),
        Alternative(true, Dollar, Some(w.clone())),
        Alternative(true, Bang, Some(w.clone())),
        Alternative(true, Positional(0), Some(w.clone())),
        Alternative(true, Positional(10), Some(w.clone())),
        Alternative(true, Positional(100), Some(w.clone())),
        Alternative(true, Var(String::from("foo_bar123")), Some(w.clone())),
        Alternative(true, At, None),
        Alternative(true, Star, None),
        Alternative(true, Pound, None),
        Alternative(true, Question, None),
        Alternative(true, Dash, None),
        Alternative(true, Dollar, None),
        Alternative(true, Bang, None),
        Alternative(true, Positional(0), None),
        Alternative(true, Positional(10), None),
        Alternative(true, Positional(100), None),
        Alternative(true, Var(String::from("foo_bar123")), None),
    ];

    let src = "${@:+foo}${*:+foo}${#:+foo}${?:+foo}${-:+foo}${$:+foo}${!:+foo}${0:+foo}${10:+foo}${100:+foo}${foo_bar123:+foo}${@:+}${*:+}${#:+}${?:+}${-:+}${$:+}${!:+}${0:+}${10:+}${100:+}${foo_bar123:+}";
    let mut p = make_parser(src);
    for s in substs {
        assert_eq!(word(subst(s)), p.parameter().unwrap());
    }
    assert_eq!(Err(UnexpectedEOF.into()), p.parameter()); // Stream should be exhausted

    let substs = vec![
        Alternative(false, At, Some(w.clone())),
        Alternative(false, Star, Some(w.clone())),
        Alternative(false, Pound, Some(w.clone())),
        Alternative(false, Question, Some(w.clone())),
        Alternative(false, Dash, Some(w.clone())),
        Alternative(false, Dollar, Some(w.clone())),
        Alternative(false, Bang, Some(w.clone())),
        Alternative(false, Positional(0), Some(w.clone())),
        Alternative(false, Positional(10), Some(w.clone())),
        Alternative(false, Positional(100), Some(w.clone())),
        Alternative(false, Var(String::from("foo_bar123")), Some(w)),
        Alternative(false, At, None),
        Alternative(false, Star, None),
        Alternative(false, Pound, None),
        Alternative(false, Question, None),
        Alternative(false, Dash, None),
        Alternative(false, Dollar, None),
        Alternative(false, Bang, None),
        Alternative(false, Positional(0), None),
        Alternative(false, Positional(10), None),
        Alternative(false, Positional(100), None),
        Alternative(false, Var(String::from("foo_bar123")), None),
    ];

    let src = "${@+foo}${*+foo}${#+foo}${?+foo}${-+foo}${$+foo}${!+foo}${0+foo}${10+foo}${100+foo}${foo_bar123+foo}${@+}${*+}${#+}${?+}${-+}${$+}${!+}${0+}${10+}${100+}${foo_bar123+}";
    let mut p = make_parser(src);
    for s in substs {
        assert_eq!(word(subst(s)), p.parameter().unwrap());
    }
    assert_eq!(Err(UnexpectedEOF.into()), p.parameter()); // Stream should be exhausted
}

#[test]
fn test_parameter_substitution_words_can_have_spaces_and_escaped_curlies() {
    let var = Var(String::from("foo_bar123"));
    let w = words(&[
        lit("foo{"),
        escaped("}"),
        lit(" \t\r "),
        escaped("\n"),
        lit("bar \t\r "),
    ]);

    let substs = vec![
        RemoveSmallestSuffix(var.clone(), Some(w.clone())),
        RemoveLargestSuffix(var.clone(), Some(w.clone())),
        RemoveSmallestPrefix(var.clone(), Some(w.clone())),
        RemoveLargestPrefix(var.clone(), Some(w.clone())),
        Default(true, var.clone(), Some(w.clone())),
        Default(false, var.clone(), Some(w.clone())),
        Assign(true, var.clone(), Some(w.clone())),
        Assign(false, var.clone(), Some(w.clone())),
        Error(true, var.clone(), Some(w.clone())),
        Error(false, var.clone(), Some(w.clone())),
        Alternative(true, var.clone(), Some(w.clone())),
        Alternative(false, var, Some(w)),
    ];

    let src = [
        "%", "%%", "#", "##", ":-", "-", ":=", "=", ":?", "?", ":+", "+",
    ];

    for (i, s) in substs.into_iter().enumerate() {
        let src = format!("${{foo_bar123{}foo{{\\}} \t\r \\\nbar \t\r }}", src[i]);
        let mut p = make_parser(&src);
        assert_eq!(word(subst(s)), p.parameter().unwrap());
        assert_eq!(Err(UnexpectedEOF.into()), p.parameter()); // Stream should be exhausted
    }
}

#[test]
fn test_parameter_substitution_words_can_start_with_pound() {
    let var = Var(String::from("foo_bar123"));
    let w = words(&[
        lit("#foo{"),
        escaped("}"),
        lit(" \t\r "),
        escaped("\n"),
        lit("bar \t\r "),
    ]);

    let substs = vec![
        RemoveSmallestSuffix(var.clone(), Some(w.clone())),
        RemoveLargestSuffix(var.clone(), Some(w.clone())),
        //RemoveSmallestPrefix(var.clone(), Some(word.clone())),
        RemoveLargestPrefix(var.clone(), Some(w.clone())),
        Default(true, var.clone(), Some(w.clone())),
        Default(false, var.clone(), Some(w.clone())),
        Assign(true, var.clone(), Some(w.clone())),
        Assign(false, var.clone(), Some(w.clone())),
        Error(true, var.clone(), Some(w.clone())),
        Error(false, var.clone(), Some(w.clone())),
        Alternative(true, var.clone(), Some(w.clone())),
        Alternative(false, var, Some(w)),
    ];

    let src = [
        "%", "%%", //"#", // Let's not confuse the pound in the word with a substitution
        "##", ":-", "-", ":=", "=", ":?", "?", ":+", "+",
    ];

    for (i, s) in substs.into_iter().enumerate() {
        let src = format!("${{foo_bar123{}#foo{{\\}} \t\r \\\nbar \t\r }}", src[i]);
        let mut p = make_parser(&src);
        assert_eq!(word(subst(s)), p.parameter().unwrap());
        assert_eq!(Err(UnexpectedEOF.into()), p.parameter()); // Stream should be exhausted
    }
}

#[test]
fn test_parameter_substitution_words_can_be_parameters_or_substitutions_as_well() {
    let v = Var(String::from("foo_bar123"));
    let w = words(&[
        param(At),
        subst(RemoveLargestPrefix(
            Var("foo".into()),
            Some(word_lit("bar")),
        )),
    ]);

    let substs = vec![
        RemoveSmallestSuffix(v.clone(), Some(w.clone())),
        RemoveLargestSuffix(v.clone(), Some(w.clone())),
        RemoveSmallestPrefix(v.clone(), Some(w.clone())),
        RemoveLargestPrefix(v.clone(), Some(w.clone())),
        Default(true, v.clone(), Some(w.clone())),
        Default(false, v.clone(), Some(w.clone())),
        Assign(true, v.clone(), Some(w.clone())),
        Assign(false, v.clone(), Some(w.clone())),
        Error(true, v.clone(), Some(w.clone())),
        Error(false, v.clone(), Some(w.clone())),
        Alternative(true, v.clone(), Some(w.clone())),
        Alternative(false, v, Some(w)),
    ];

    let src = [
        "%", "%%", "#", "##", ":-", "-", ":=", "=", ":?", "?", ":+", "+",
    ];

    for (i, s) in substs.into_iter().enumerate() {
        let src = format!("${{foo_bar123{}$@${{foo##bar}}}}", src[i]);
        let mut p = make_parser(&src);
        assert_eq!(word(subst(s)), p.parameter().unwrap());
        assert_eq!(Err(UnexpectedEOF.into()), p.parameter()); // Stream should be exhausted
    }
}

#[test]
fn test_parameter_substitution_command_close_paren_need_not_be_followed_by_word_delimeter() {
    let correct = Some(cmd_from_words(
        "foo",
        &[word(subst(Command(vec![cmd("bar")])))],
    ));
    assert_eq!(
        correct,
        make_parser("foo \"$(bar)\"").complete_command().unwrap()
    );
}

#[test]
fn test_parameter_substitution_invalid() {
    let cases = vec![
        ("$(( x", UnexpectedEOF.into()),
        ("${foo", Unmatched(Token::CurlyOpen, src(1, 1, 2)).into()),
        (
            "${ foo}",
            BadSubst(Token::Whitespace(String::from(" ")), src(2, 1, 3)).into(),
        ),
        (
            "${foo }",
            BadSubst(Token::Whitespace(String::from(" ")), src(5, 1, 6)).into(),
        ),
        (
            "${foo -}",
            BadSubst(Token::Whitespace(String::from(" ")), src(5, 1, 6)).into(),
        ),
        (
            "${foo =}",
            BadSubst(Token::Whitespace(String::from(" ")), src(5, 1, 6)).into(),
        ),
        (
            "${foo ?}",
            BadSubst(Token::Whitespace(String::from(" ")), src(5, 1, 6)).into(),
        ),
        (
            "${foo +}",
            BadSubst(Token::Whitespace(String::from(" ")), src(5, 1, 6)).into(),
        ),
        (
            "${foo :-}",
            BadSubst(Token::Whitespace(String::from(" ")), src(5, 1, 6)).into(),
        ),
        (
            "${foo :=}",
            BadSubst(Token::Whitespace(String::from(" ")), src(5, 1, 6)).into(),
        ),
        (
            "${foo :?}",
            BadSubst(Token::Whitespace(String::from(" ")), src(5, 1, 6)).into(),
        ),
        (
            "${foo :+}",
            BadSubst(Token::Whitespace(String::from(" ")), src(5, 1, 6)).into(),
        ),
        (
            "${foo: -}",
            BadSubst(Token::Whitespace(String::from(" ")), src(6, 1, 7)).into(),
        ),
        (
            "${foo: =}",
            BadSubst(Token::Whitespace(String::from(" ")), src(6, 1, 7)).into(),
        ),
        (
            "${foo: ?}",
            BadSubst(Token::Whitespace(String::from(" ")), src(6, 1, 7)).into(),
        ),
        (
            "${foo: +}",
            BadSubst(Token::Whitespace(String::from(" ")), src(6, 1, 7)).into(),
        ),
        (
            "${foo: %}",
            BadSubst(Token::Whitespace(String::from(" ")), src(6, 1, 7)).into(),
        ),
        (
            "${foo: #}",
            BadSubst(Token::Whitespace(String::from(" ")), src(6, 1, 7)).into(),
        ),
        (
            "${foo-bar",
            Unmatched(Token::CurlyOpen, src(1, 1, 2)).into(),
        ),
        (
            "${'foo'}",
            BadSubst(Token::SingleQuote, src(2, 1, 3)).into(),
        ),
        (
            "${\"foo\"}",
            BadSubst(Token::DoubleQuote, src(2, 1, 3)).into(),
        ),
        ("${`foo`}", BadSubst(Token::Backtick, src(2, 1, 3)).into()),
    ];

    for (s, correct) in cases.into_iter() {
        match make_parser(s).parameter() {
            Ok(w) => panic!("Unexpectedly parsed the source \"{}\" as\n{:?}", s, w),
            Err(ref err) => {
                if err != &correct {
                    panic!(
                        "Expected the source \"{}\" to return the error `{:?}`, but got `{:?}`",
                        s, correct, err
                    );
                }
            }
        }
    }
}

#[test]
fn test_parameter_substitution_nested_quoted() {
    let param = Var("foo".to_owned());
    let cases = vec![
        (
            "${foo:+'bar'}",
            Alternative(true, param.clone(), Some(word(lit("bar")))),
        ),
        (
            "${foo:+\"bar\"}",
            Alternative(true, param.clone(), Some(word_lit("bar"))),
        ),
        (
            "${foo:+`bar`}",
            Alternative(true, param, Some(word(subst(Command(vec![cmd("bar")]))))),
        ),
    ];

    for (src, s) in cases.into_iter() {
        let correct = word(subst(s));
        let parsed = make_parser(src).parameter();
        if parsed.as_ref() != Ok(&correct) {
            panic!(
                "Expected \"{}\" to parse as `{:?}`, but got `{:?}`",
                src, correct, parsed
            );
        }
    }
}

#[test]
fn test_parameter_substitution_can_have_nested_substitution_and_parameter() {
    let param_foo = Var("foo".to_owned());
    let param_bar = Var("bar".to_owned());
    let correct = word(subst(Alternative(
        true,
        param_foo,
        Some(word(subst(Alternative(
            true,
            param_bar,
            Some(word(param(Dollar))),
        )))),
    )));

    let mut p = make_parser("${foo:+${bar:+$$}}");
    assert_eq!(Ok(correct), p.parameter());
}

#[test]
fn test_parameter_substitution_special_tokens_in_words_become_literals() {
    let correct = word(subst(Default(
        true,
        Var("foo".to_owned()),
        Some(words(&[
            lit("#(bar);&|&&||;; << >> <& >& <<- "),
            escaped("\n"),
            lit("\n\t"),
        ])),
    )));

    let mut p = make_parser("${foo:-#(bar);&|&&||;; << >> <& >& <<- \\\n\n\t}");
    assert_eq!(Ok(correct), p.parameter());
}
