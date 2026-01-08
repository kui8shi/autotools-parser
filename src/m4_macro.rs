//! Provide data structures related to m4 macros.
use crate::ast::condition::Condition;
use std::collections::HashMap;
use ArrayDelim::*;
use M4ExportType::*;
use M4Type::*;

/// Specify types of arguments or expansion of m4 macro calls.
#[derive(Clone, Copy)]
pub enum M4Type {
    /// raw literal treated as is.
    Lit,
    /// parsed as a word
    Word,
    /// array of words separated by whitespace
    Arr(ArrayDelim),
    /// array of macro arguments separated by comma
    Args,
    /// represents a program to be used for checking by compiling it
    Prog,
    /// list of shell script or m4 macro
    Cmds,
    /// result of macro definition
    Def,
    /// control macro processor, such as changing quote characters.
    Ctrl,
    /// body of macro definition
    Body,
    /// path string, while some variables based on the value may be generated.
    /// a colon-separated list of paths can be appended to a path string.
    /// e.g. path1:path2:path3
    Path(Option<M4ExportFunc>),
    /// array of path strings separated by whitespace.
    Paths(ArrayDelim, Option<M4ExportFunc>),
    /// the type string include struct member strings, e.g. `struct A.member`
    Type(Option<M4ExportFunc>),
    /// array of type strings separated by comma (expecting the array to be enclosed by quotes)
    Types(ArrayDelim, Option<M4ExportFunc>),
    /// output shell variable name, conversions may be applied to create other variable names.
    VarName(Option<VarAttrs>, Option<M4ExportFunc>),
    /// library name , conversions may be applied to become variable names.
    Library(Option<M4ExportFunc>),
    /// C preprocessor symbol name, conversions may be applied to become variable names.
    CPPSymbol,
    /// C symbol, conversions may be applied to become variable names.
    Symbol(Option<M4ExportFunc>),
    /// array of C symbols, separated by whitespace.
    Symbols(ArrayDelim, Option<M4ExportFunc>),
    /// condition to define Automake conditional variable
    AMCond,
}

impl M4Type {
    /// Return whether the argument has any implicit side effects.
    pub fn has_no_exports(&self) -> bool {
        match self {
            Path(Some(_))
            | Paths(_, Some(_))
            | Type(Some(_))
            | Types(_, Some(_))
            | VarName(_, Some(_))
            | Library(Some(_))
            | Symbol(Some(_))
            | Symbols(_, Some(_)) => false,
            _ => true,
        }
    }
}

/// A lambda function exporging side effects.
pub type M4ExportFunc = &'static (dyn Fn(&str) -> Vec<M4ExportType> + Sync);

/// The type of any dynamically exported information
#[derive(Debug, Clone)]
pub enum M4ExportType {
    /// Export a shell variable
    ExVar(VarAttrs, String, Option<String>),
    /// Export a C preprocessor symbol
    ExCPP(String, Option<Option<String>>),
    /// Reference to a path
    ExPath(String),
    /// Represent a tag b/w two paths
    /// Mainly for <path>:<path> syntax
    ExTag(String, Option<String>),
    /// Export to automake via AM_CONDITIONAL
    ExCond(String),
}

impl PartialEq for M4Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Lit, Lit) => true,
            (Word, Word) => true,
            (Arr(_), Arr(_)) => true,
            (Args, Args) => true,
            (Prog, Prog) => true,
            (Cmds, Cmds) => true,
            (Def, Def) => true,
            (Ctrl, Ctrl) => true,
            (Body, Body) => true,
            (Path(_), Path(_)) => true,
            (Paths(_, _), Paths(_, _)) => true,
            (Type(_), Type(_)) => true,
            (Types(_, _), Types(_, _)) => true,
            (VarName(_, _), VarName(_, _)) => true,
            (Library(_), Library(_)) => true,
            (CPPSymbol, CPPSymbol) => true,
            (Symbol(_), Symbol(_)) => true,
            (Symbols(_, _), Symbols(_, _)) => true,
            (AMCond, AMCond) => true,
            _ => false,
        }
    }
}

impl std::fmt::Debug for M4Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Lit => "Lit",
            Word => "Word",
            Arr(_) => "Arr",
            Args => "Args",
            Prog => "Prog",
            Cmds => "Cmds",
            Def => "Def",
            Ctrl => "Ctrl",
            Body => "Body",
            Path(_) => "Path",
            Paths(_, _) => "Paths",
            Type(_) => "Type",
            Types(_, _) => "Types",
            VarName(_, _) => "Var",
            Library(_) => "Library",
            CPPSymbol => "CPP",
            Symbol(_) => "Symbol",
            Symbols(_, _) => "Symbols",
            AMCond => "AMCond",
        })
    }
}

/// Represents an argument of m4 macro call.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum M4Argument<C, W> {
    /// raw literal
    Literal(String),
    /// a shell word, likes a parsed version of the literal
    Word(W),
    /// array of words
    Array(Vec<W>),
    /// program string
    Program(String),
    /// list of commands.
    Commands(Vec<C>),
    /// condition (e.g test command).
    Condition(Condition<C, W>),
    /// unknown argument type when the macro is user-defined
    Unknown(String),
}

// @kui8shi
/// Represents a m4 macro call.
///
/// M4 macros can be inserted at literally anywhere.
/// However, we only support 2 places:
/// 1. CompoundCommand
/// 2. SimpleWord
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct M4Macro<C, W> {
    /// m4 macro name
    pub name: String,
    /// m4 macro arguments
    pub args: Vec<M4Argument<C, W>>,
    /// side effects from the call
    pub effects: Option<SideEffect>,
    /// original m4 macro name if an alternative macro was adopted
    pub original_name: Option<String>,
    /// additional information about this macro
    pub signature: Option<M4MacroSignature>,
}

impl<C, W> M4Macro<C, W> {
    /// Create a new M4 macro call node.
    pub fn new(name: String, args: Vec<M4Argument<C, W>>) -> Self {
        Self::new_with_side_effect(name, args, None, None)
    }

    /// Create a new M4 macro call node with information about side effects.
    pub fn new_with_side_effect(
        name: String,
        args: Vec<M4Argument<C, W>>,
        effects: Option<SideEffect>,
        original_name: Option<String>,
    ) -> Self {
        let signature = MACROS.get(&name).cloned();
        Self {
            name,
            args,
            effects,
            original_name,
            signature,
        }
    }
}

impl<C: Clone, W: Clone> M4Macro<C, W> {
    /// Take the scpeficied argument as a command
    pub fn get_arg_as_cmd(&self, index: usize) -> Option<Vec<C>> {
        match self.args.get(index) {
            Some(M4Argument::Commands(cmds)) => Some(cmds.to_vec()),
            _ => None,
        }
    }

    /// Take the scpeficied argument as a word
    pub fn get_arg_as_word(&self, index: usize) -> Option<W> {
        match self.args.get(index) {
            Some(M4Argument::Word(word)) => Some(word.clone()),
            _ => None,
        }
    }

    /// Take the scpeficied argument as an array of words
    pub fn get_arg_as_array(&self, index: usize) -> Option<Vec<W>> {
        match self.args.get(index) {
            Some(M4Argument::Array(words)) => Some(words.clone()),
            _ => None,
        }
    }

    /// Take the scpeficied argument as a literal
    pub fn get_arg_as_literal(&self, index: usize) -> Option<String> {
        match self.args.get(index) {
            Some(M4Argument::Literal(lit)) => Some(lit.clone()),
            _ => None,
        }
    }

    /// Take the scpeficied argument as a program
    pub fn get_arg_as_program(&self, index: usize) -> Option<String> {
        match self.args.get(index) {
            Some(M4Argument::Program(prog)) => Some(prog.clone()),
            _ => None,
        }
    }
}

/// Represent side effects that an operation (e.g. macro call) could produce.
#[derive(Debug, PartialEq, Eq, Clone, Default)]
pub struct SideEffect {
    /// exported shell variables.
    pub shell_vars: Option<Vec<Var>>,
    /// exported shell variables.
    pub cpp_symbols: Option<Vec<CPP>>,
    /// exported path strings.
    pub paths: Option<Vec<String>>,
    /// exported tags of path strings.
    pub tags: Option<Vec<(String, Option<String>)>>,
    /// exported automake conditional variables.
    pub am_conds: Option<Vec<String>>,
}

impl From<&M4MacroSignature> for SideEffect {
    fn from(value: &M4MacroSignature) -> Self {
        SideEffect {
            shell_vars: value.shell_vars.clone(),
            cpp_symbols: value.cpp_symbols.clone(),
            paths: value.paths.clone(),
            ..Default::default()
        }
    }
}

impl SideEffect {
    /// A shell variable is defined.
    pub fn add_shell_var(&mut self, val: &str, attrs: &VarAttrs, value: &Option<String>) {
        let var = Var {
            name: val.into(),
            attrs: *attrs,
            value: value.clone(),
        };
        if let Some(v) = &mut self.shell_vars {
            v.push(var)
        } else {
            self.shell_vars = Some(vec![var])
        }
    }

    /// A cpp symbol is defined.
    pub fn add_cpp_symbol(&mut self, val: &str, value: &Option<Option<String>>) {
        let cpp = CPP::new(val, value.clone());
        if let Some(v) = &mut self.cpp_symbols {
            v.push(cpp)
        } else {
            self.cpp_symbols = Some(vec![cpp])
        }
    }

    /// A path is touched.
    pub fn add_path(&mut self, val: &str) {
        if let Some(v) = &mut self.paths {
            v.push(val.into())
        } else {
            self.paths = Some(vec![val.into()])
        }
    }

    /// A tag b/w two paths is defined.
    pub fn add_tag(&mut self, dst: &str, src: &Option<String>) {
        if let Some(v) = &mut self.tags {
            v.push((dst.into(), src.to_owned()))
        } else {
            self.tags = Some(vec![(dst.into(), src.to_owned())])
        }
    }

    /// An automake conditional variable is defined.
    pub fn add_am_cond(&mut self, val: &str) {
        if let Some(v) = &mut self.am_conds {
            v.push(val.into())
        } else {
            self.am_conds = Some(vec![val.into()])
        }
    }

    /// Any type of export is triggered.
    pub fn add_side_effect(&mut self, export_type: &M4ExportType) {
        match export_type {
            ExVar(attrs, var, value) => self.add_shell_var(var, attrs, value),
            ExCPP(symbol, value) => self.add_cpp_symbol(symbol, value),
            ExPath(path) => self.add_path(path),
            ExTag(dst, src) => self.add_tag(dst, src),
            ExCond(cond) => self.add_am_cond(cond),
        }
    }
}

/// Represent the external and internal specs of a m4 macro
#[derive(Debug, Clone, Default)]
pub struct M4MacroSignature {
    /// the types of macro arguments.
    pub arg_types: Vec<M4Type>,
    /// the number of required arguments. rest of arguments are optional.
    /// this is useful especially for macros with confusing names (e.g. define).
    pub num_args_required: usize,
    /// the type of macro after expansion.
    pub ret_type: Option<M4Type>,
    /// repeat the part of arg_types[start..=end] as long as arguments are given.
    pub repeat: Option<(usize, usize)>,
    /// shell variables affected/used by the macro.
    pub shell_vars: Option<Vec<Var>>,
    /// c preprocessor symbols defined by the macro.
    pub cpp_symbols: Option<Vec<CPP>>,
    /// automake conditionals defined by the macro.
    pub am_conds: Option<Vec<AmCond>>,
    /// When the macro is obsolete and completely replaced by another macro.
    /// If this field is some, other fields should be empty or none.
    pub replaced_by: Option<String>,
    /// List of macro names to be called without arguments if not. c.f. AC_REQUIRE
    /// We expect required macros have the same return type, and no arguments.
    pub require: Option<Vec<String>>,
    /// If the macro should be called only once in the entire script.
    pub is_oneshot: bool,
    /// List of paths referenced in the macro
    pub paths: Option<Vec<String>>,
}

impl PartialEq for M4MacroSignature {
    fn eq(&self, other: &Self) -> bool {
        for (a, b) in self.arg_types.iter().zip(other.arg_types.iter()) {
            if a != b {
                return false;
            }
        }
        self.num_args_required == other.num_args_required
            && self.ret_type == other.ret_type
            && self.repeat == other.repeat
            && self.shell_vars == other.shell_vars
            && self.cpp_symbols == other.cpp_symbols
            && self.replaced_by == other.replaced_by
            && self.require == other.require
            && self.paths == other.paths
    }
}

impl Eq for M4MacroSignature {}

impl M4MacroSignature {
    /// Return whether the signature has any explicit side effects.
    pub fn has_no_exports(&self) -> bool {
        self.shell_vars.is_none() && self.cpp_symbols.is_none() && self.paths.is_none()
    }
}

/// Represents a static shell variable
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Var {
    /// name of shell variable
    pub name: String,
    /// attributes linked to the variable
    pub attrs: VarAttrs,
    /// the variable value if fixed
    pub value: Option<String>,
}

impl Var {
    /// return if it has input attribute
    pub fn is_input(&self) -> bool {
        match self.attrs.kind {
            VarKind::Input | VarKind::Precious | VarKind::Environment => true,
            _ => false,
        }
    }
    /// return if it has output attribute
    pub fn is_output(&self) -> bool {
        match self.attrs.kind {
            VarKind::Output | VarKind::Precious | VarKind::Environment => true,
            _ => false,
        }
    }

    /// return if it has environmental attribute
    pub fn is_env(&self) -> bool {
        match self.attrs.kind {
            VarKind::Environment => true,
            _ => false,
        }
    }

    /// return if it is referenced
    pub fn is_used(&self) -> bool {
        match self.attrs.usage {
            VarUsage::Referenced | VarUsage::Append => true,
            _ => false,
        }
    }

    /// return if it is defined
    pub fn is_defined(&self) -> bool {
        match self.attrs.usage {
            VarUsage::Defined | VarUsage::Append => true,
            _ => false,
        }
    }

    /// return if it has fixed value
    pub fn has_fixed_value(&self) -> bool {
        self.value.is_some()
    }
}

impl Var {
    /// Create a new shell variable with specified attributes
    pub fn new(name: &str, kind: VarKind, usage: VarUsage, value: Option<&str>) -> Self {
        Self {
            name: name.into(),
            attrs: VarAttrs::new(kind, usage),
            value: value.map(|s| s.to_owned()),
        }
    }

    /// Create a new shell variable reference
    pub fn reference(name: &str) -> Self {
        Self::new(name, VarKind::Internal, VarUsage::Referenced, None)
    }

    /// Create a new shell variable reference with its kind known
    pub fn reference_some(name: &str, kind: VarKind) -> Self {
        Self::new(name, kind, VarUsage::Referenced, None)
    }

    /// Create a new internal shell variable
    pub fn define_internal(name: &str) -> Self {
        Self::new(name, VarKind::Internal, VarUsage::Defined, None)
    }

    /// Create a new output shell variable
    pub fn define_output(name: &str) -> Self {
        Self::new(name, VarKind::Output, VarUsage::Defined, None)
    }

    /// Create a new input shell variable
    pub fn define_input(name: &str) -> Self {
        Self::new(name, VarKind::Input, VarUsage::Defined, None)
    }

    /// Create a new input+output shell variable
    pub fn define_precious(name: &str) -> Self {
        Self::new(name, VarKind::Precious, VarUsage::Defined, None)
    }

    /// Create a new environmental variable
    pub fn define_env(name: &str) -> Self {
        Self::new(name, VarKind::Environment, VarUsage::Defined, None)
    }

    /// Create a shell variable adding operation
    pub fn append(name: &str, kind: VarKind) -> Self {
        Self::new(name, kind, VarUsage::Append, None)
    }

    /// Create a shell variable with value fixed
    pub fn with_value(mut self, value: &str) -> Self {
        self.value.replace(value.into());
        self
    }

    /// Create a shell variable with value fixed to "yes"
    pub fn yes(self) -> Self {
        self.with_value("yes")
    }

    /// Create a shell variable with value fixed to "yes"
    pub fn no(self) -> Self {
        self.with_value("no")
    }
}

impl From<&str> for Var {
    fn from(value: &str) -> Self {
        Self::new(value, VarKind::Internal, VarUsage::Defined, None)
    }
}

/// Represents a C preprocessor symbol
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct CPP {
    /// name of symbol
    pub name: String,
    /// the symbol's value if fixed.
    /// None if not fixed.
    /// Some(None) if fixed to unset.
    /// Some(Some(_)) if fixed to specific value.
    /// Note that we assume latest environment (e.g. linux-gnu) for this.
    pub value: Option<Option<String>>,
}

impl CPP {
    /// return if it has fixed value
    pub fn has_fixed_value(&self) -> bool {
        self.value.is_some()
    }
}

impl CPP {
    /// Create a new cpp symbol
    pub fn new<S: AsRef<str>>(name: &str, value: Option<Option<S>>) -> Self {
        Self {
            name: name.into(),
            value: value.map(|o| o.map(|s| s.as_ref().to_owned())),
        }
    }

    /// Create a new cpp symbol with specified value
    pub fn with_value(name: &str, value: &str) -> Self {
        Self::new(name, Some(Some(value)))
    }

    /// Create a new shell variable with "1" value
    pub fn set(name: &str) -> Self {
        Self::new(name, Some(Some("1")))
    }

    /// Create a new shell variable with unset
    pub fn unset(name: &str) -> Self {
        Self::new::<&str>(name, Some(None))
    }
}

impl From<&str> for CPP {
    fn from(name: &str) -> Self {
        Self {
            name: name.to_owned(),
            value: None,
        }
    }
}

/// Represents an automake conditoinals
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct AmCond {
    /// name of conditional
    pub name: String,
    /// the boolean value if fixed.
    pub value: Option<bool>,
}

impl AmCond {
    /// return if it has fixed value
    pub fn has_fixed_value(&self) -> bool {
        self.value.is_some()
    }
}

impl AmCond {
    /// Create a new cpp symbol
    pub fn new(name: &str) -> Self {
        Self {
            name: name.into(),
            value: None,
        }
    }

    /// Create a new shell variable with "1" value
    pub fn set(mut self) -> Self {
        self.value.replace(true);
        self
    }

    /// Create a new shell variable with unset
    pub fn unset(mut self) -> Self {
        self.value.replace(false);
        self
    }
}

impl From<&str> for AmCond {
    fn from(name: &str) -> Self {
        Self {
            name: name.to_owned(),
            value: None,
        }
    }
}
/// Represents a type of a shell variable
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VarKind {
    /// internally used in the script
    Internal,
    /// very close to the configure script's argument
    Input,
    /// called AC_SUBST on it
    Output,
    /// input & output
    Precious,
    /// exported to an environmental variable
    Environment,
}

impl Default for VarKind {
    fn default() -> Self {
        Self::Internal
    }
}

/// Represents an usage of a shell variable
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VarUsage {
    /// variable is only read
    Referenced,
    /// variable is defined
    Defined,
    /// variable is like a list and an item is added
    Append,
}

impl Default for VarUsage {
    fn default() -> Self {
        Self::Defined
    }
}

/// Represents attributes of a shell variable
#[derive(Debug, Clone, Default, Copy, PartialEq, Eq)]
pub struct VarAttrs {
    /// about the data flow of the variable
    pub kind: VarKind,
    /// about the control of the variable
    pub usage: VarUsage,
}

impl VarAttrs {
    /// Create a new shell variable attributes
    pub fn new(kind: VarKind, usage: VarUsage) -> Self {
        Self { kind, usage }
    }

    /// Create a new attributes for reading operations
    pub fn read(kind: Option<VarKind>) -> Self {
        Self::new(kind.unwrap_or(VarKind::Internal), VarUsage::Referenced)
    }

    /// Create a new attributes for appending operations
    pub fn append(kind: Option<VarKind>) -> Self {
        Self::new(kind.unwrap_or(VarKind::Internal), VarUsage::Append)
    }

    /// Create a new internal shell variable attributes
    pub fn internal() -> Self {
        Self::new(VarKind::Internal, VarUsage::Defined)
    }

    /// Create a new output shell variable attributes
    pub fn output() -> Self {
        Self::new(VarKind::Output, VarUsage::Defined)
    }

    /// Create a new input shell variable attributes
    pub fn input() -> Self {
        Self::new(VarKind::Input, VarUsage::Defined)
    }

    /// Create a new input shell variable attributes
    pub fn precious() -> Self {
        Self::new(VarKind::Precious, VarUsage::Defined)
    }

    /// Create a new environmental variable attributes
    pub fn env() -> Self {
        Self::new(VarKind::Environment, VarUsage::Defined)
    }
}

/// Delimiter used in a m4 array argument
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum ArrayDelim {
    /// whitespace or newline
    Blank,
    /// ','
    Comma,
}

fn sanitize_c_name(s: &str) -> String {
    s.chars()
        .map(|c| {
            if c.is_ascii_alphanumeric() {
                c.to_uppercase().next().unwrap()
            } else if c == '*' {
                'P'
            } else {
                '_'
            }
        })
        .collect()
}

fn sanitize_shell_name(s: &str) -> String {
    s.chars()
        .map(|c| if c.is_ascii_alphanumeric() { c } else { '_' })
        .collect()
}

fn split_tag(tag: &str) -> Vec<M4ExportType> {
    let mut split = tag.split(":");
    let dst = split.next().map(|s| s.to_owned()).unwrap();
    let src = split.next().map(|s| s.to_owned());
    vec![ExTag(dst, src)]
}

/// Return macro signature if a predefined macro found.
pub fn get_macro(name: &str) -> Option<(&String, &M4MacroSignature, Option<&String>)> {
    MACROS.get_key_value(name).map(|(key, signature)| {
        if let Some(ref alternative) = signature.replaced_by {
            let signature = MACROS.get(alternative).unwrap();
            (alternative, signature, Some(key))
        } else {
            (key, signature, None)
        }
    })
}

// pub fn get_macros() -> HashMap<&'static str, M4MacroSignature> {
// arg_types, ret_type, repeat, output_variables, preprocessor_symbols
// TODO: revert to the static definition style.

lazy_static::lazy_static! {
    /// Predefined m4/autoconf macros
    pub static ref MACROS: HashMap<String, M4MacroSignature> = predefined_macros();
}

fn predefined_macros() -> HashMap<String, M4MacroSignature> {
    use VarKind::*;
    use VarUsage::*;
    HashMap::from(
        [
            // Initializing configure
            (
                "AC_INIT",
                M4MacroSignature {
                    arg_types: vec![
                        Lit,  // package
                        Word, // version
                        Lit,  // [bug-report]
                        Word, // [tarname]
                        Lit,  // [url]
                    ],
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        Var::define_output("PACKAGE_NAME"),
                        Var::define_output("PACKAGE_TARNAME"),
                        Var::define_output("PACKAGE_VERSION"),
                        Var::define_output("PACKAGE_STRING"),
                        Var::define_output("PACKAGE_BUGREPORT"),
                        Var::define_output("PACKAGE_URL"),
                        // Internal Preset
                        // by _AC_INIT_DEFAULTS
                        "ac_host_name".into(),
                        Var::define_internal("ac_default_prefix").with_value("/usr/local"), // /usr/local
                        "ac_clean_CONFIG_STATUS".into(),
                        "ac_clean_files".into(),
                        "ac_config_libobj_dir".into(),
                        "LIBOBJS".into(),
                        "subdirs".into(),
                        "MFLAGS".into(),
                        "MAKEFLAGS".into(),
                        Var::define_output("SHELL").with_value("/bin/sh"),
                        Var::define_env("PATH_SEPARATOR").with_value(":"),
                        // by _AC_INIT_PARSE_ARGS
                        Var::define_input("cache_file"), // --cache-file
                        Var::define_input("with_gas"),   // --with-gas
                        Var::define_input("with_fp"),    // --without-fp
                        Var::define_input("with_x"),     // --with-x
                        Var::define_input("no_create"),  // --no-create
                        Var::define_input("no_recursion"), // --no-recusion
                        Var::define_input("ac_init_help"), // --help
                        Var::define_input("ac_init_version"), // --version
                        Var::define_input("silent"),     // --silent
                        Var::define_input("site"),       // --silent
                        Var::define_input("verbose"),    // --verbose
                        Var::define_input("x_includes"), // --x-includes
                        Var::define_input("x_libraries"), // --x-libraries
                        Var::define_input("prefix"),     // --prefix
                        Var::define_input("program_prefix"), // --program-prefix
                        Var::define_input("program_suffix"), // --program-suffix
                        Var::define_input("program_transform_name"), // --program-transform-name
                        Var::define_input("program_suffix"), // --program-suffix
                        Var::define_input("exec_prefix").with_value("/usr/local"), // = $prefix
                        Var::define_precious("srcdir").with_value("."), // --srcdir
                        Var::define_precious("bindir").with_value("/usr/local/bin"), // ${exec_prefix}/bin
                        Var::define_precious("sbindir").with_value("/usr/local/sbin"), // ${exec_prefix}/sbin
                        Var::define_precious("libexecdir").with_value("/usr/local/libexec"), // ${exec_prefix}/libexec
                        Var::define_precious("datarootdir").with_value("/usr/local/share"), // ${prefix}/share
                        Var::define_precious("datadir").with_value("/usr/local/share"), // ${datarootdir}
                        Var::define_precious("sysconfdir").with_value("/usr/local/etc"), // ${prefix}/etc
                        Var::define_precious("sharedstatedir").with_value("/usr/local/com"), // ${prefix}/com
                        Var::define_precious("localstatedir").with_value("/usr/local/var"), // ${prefix}/var
                        Var::define_precious("runstatedir").with_value("/usr/local/var/run"), // ${localstatedir}/run
                        Var::define_precious("includedir").with_value("/usr/local/include"), // ${prefix}/include
                        Var::define_precious("oldincludedir").with_value("/usr/include"), // /usr/include
                        Var::define_precious("docdir"), // ${datarootdir}/doc/${PACKAGE}
                        Var::define_precious("infodir").with_value("/usr/local/share/info"), // ${datarootdir}/info
                        Var::define_precious("htmldir"), // ${docdir}
                        Var::define_precious("dvidir"),  // ${docdir}
                        Var::define_precious("pdfdir"),  // ${docdir}
                        Var::define_precious("psdir"),   // ${docdir}
                        Var::define_precious("libdir").with_value("/usr/local/lib"), // ${exec_prefix}/lib
                        Var::define_precious("localedir").with_value("/usr/local/share/locale"), // ${datarootdir}/locale
                        Var::define_precious("mandir").with_value("/usr/local/share/man"), // ${datarootdir}/man
                        // Var::define_precious("host"),    // $host
                        // Var::define_precious("host_alias"), // $host_alias
                        // Var::define_precious("build"),   // raw argument
                        // Var::define_precious("build_alias"), // canonicalized
                        // Var::define_precious("target"),  // $build_alias
                        // Var::define_precious("target_alias"), // $build_alias
                        Var::define_internal("cross_compiling").no(), // $host_alias != $build_alias
                        "ac_tool_prefix".into(),                      // "$host_alias-"
                        // by _AC_INIT_DIRCHECK
                        "ac_pwd".into(),
                        // by _AC_INIT_SRCDIR
                        // if $srcdir is not provided
                        "ac_srcdir_defaulted".into(),
                        "ac_abs_confdir".into(), // abs path to $srcdir
                        Var::reference("ac_unique_file"),
                        // by AC_SITE_LOAD
                        // configure will load each script file in $CONFIG_SITE
                        // by default, CONFIG_SITE="$ac_default_prefix/share/config.site
                        // $ac_default_prefix/etc/config.site"
                        Var::define_env("CONFIG_SITE"),
                        // by general.m4
                        "as_unset".into(),
                        "as_echo".into(),
                        "as_ln_s".into(),
                        "as_dir".into(),
                        Var::define_output("top_srcdir"),
                        Var::define_output("abs_top_srcdir"),
                        Var::define_output("EXEEXT").with_value(""),
                    ]),
                    cpp_symbols: Some(vec![
                        // by _AC_INIT_PREPARE
                        "PACKAGE_NAME".into(),
                        "PACKAGE_TARNAME".into(),
                        "PACKAGE_VERSION".into(),
                        "PACKAGE_STRING".into(),
                        "PACKAGE_BUGREPORT".into(),
                        "PACKAGE_URL".into(),
                    ]),
                    require: Some(vec![
                        "AS_INIT".into(),
                        "AS_ME_PREPARE".into(),
                        "AS_UNAME".into(),
                    ]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AM_INIT_AUTOMAKE",
                M4MacroSignature {
                    arg_types: vec![
                        Arr(Blank), // options
                    ],
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        Var::define_output("am__isrc").with_value("-I."), // -I$(srcdir)
                        Var::define_output("CYGPATH_W"),                 // cygpath -w, or echo
                        Var::define_output("PACKAGE"),
                        Var::define_output("VERSION"),
                        Var::define_output("mkdir_p").with_value("mkdir -p"),
                        Var::define_output("AMTAR"),
                        Var::define_output("am__tar"),
                        Var::define_output("am__untar").with_value("tar -xf -"),
                        Var::define_output("CTAGS").with_value("ctags"),
                        Var::define_output("ETAGS").with_value("etags"),
                        Var::define_output("CSCOPE").with_value("cscope"),
                        // by _AM_PROG_RM_F
                        Var::define_output("am__rm_f_notfound"),
                        // by _AM_PROG_XARGS_N
                        Var::define_output("am__xargs_n"),
                        // by calls of AM_MISSING_PROG(...)
                        Var::define_output("ACLOCAL"),
                        Var::define_output("AUTOCONF"),
                        Var::define_output("AUTOMAKE"),
                        Var::define_output("AUTOHEADER"),
                        Var::define_output("MAKEINFO"),
                        // by AM_PROG_INSTALL_SH
                        Var::define_output("install_sh"),
                        // by AM_PROG_INSTALL_STRIP
                        // INSTALL_STRIP_PROGRAM is used when `make install-strip`,
                        // overwriting INSTALL_PROGRAM.
                        Var::define_output("INSTALL_STRIP_PROGRAM").with_value(" -c -s"),
                        // by AM_SET_LEADING_DOT
                        Var::define_output("am__leading_dot").with_value("."),
                        // by AM_SET_DEPDIR
                        Var::define_output("DEPDIR").with_value(".deps"),
                        // by AM_MAKE_INCLUDE
                        Var::define_output("am__include").with_value("include"),
                        Var::define_output("am__quote").with_value(""),
                        // by AM_DEP_TRACK
                        Var::define_input("enable_dependency_tracking"),
                        Var::define_output("AMDEPBACKSLASH").with_value("\\"),
                        Var::define_output("am__nodep").with_value("_no"),
                        // by _AM_DEPENDENCIES
                        Var::define_output("CCDEPMODE").with_value("depmode=gcc3"),
                        Var::define_output("CXXDEPMODE").with_value("depmode=gcc3"),
                        Var::define_output("OBJCDEPMODE").with_value("depmode=gcc3"),
                        Var::define_output("OBJCXXDEPMODE").with_value("depmode=gcc3"),
                    ]),
                    cpp_symbols: Some(vec!["PACKAGE".into(), "VERSION".into()]),
                    am_conds: Some(vec![
                        AmCond::new("AMDEP").set(),
                        AmCond::new("am__fastdepCC").set(),
                        AmCond::new("am__fastdepCXX").set(),
                        AmCond::new("am__fastdepOBJC").set(),
                        AmCond::new("am__fastdepOBJCXX").set(),
                        // if _AM_COMPILER_EXEEXT is provided
                        AmCond::new("am__EXEEXT").unset(), // set if windows?
                    ]),
                    require: Some(vec![
                        "AC_PROG_INSTALL".into(),
                        "AC_PROG_MKDIR_P".into(),
                        "AC_PROG_AWK".into(),
                        "AC_PROG_MAKE_SET".into(),
                    ]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            // Dealing with Autoconf versions
            (
                "AC_PREREQ",
                M4MacroSignature {
                    arg_types: vec![
                        Word, // version
                    ],
                    ret_type: Some(Cmds),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AC_AUTOCONF_VERSION",
                M4MacroSignature {
                    ret_type: Some(Lit), // TODO: btw the return type should be Word?
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "m4_PACKAGE_VERSION",
                // Undocumented. Not confirmed but it must be equal to AC_AUTOCONF_VERSION
                M4MacroSignature {
                    replaced_by: Some("AC_AUTOCONF_VERSION".into()),
                    ..Default::default()
                },
            ),
            // Notices in configure
            (
                "AC_COPYRIGHT",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // copyright-notice
                    ],
                    ret_type: Some(Cmds),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AC_REVISION",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // revision-info
                    ],
                    ret_type: Some(Cmds),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            // Configure input
            (
                "AC_CONFIG_SRCDIR",
                M4MacroSignature {
                    arg_types: vec![
                        Path(None), // unique-file-in-source-dir
                    ],
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec!["ac_unique_file".into()]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AC_CONFIG_MACRO_DIR",
                M4MacroSignature {
                    arg_types: vec![
                        Path(None), // dir
                    ],
                    ret_type: Some(Cmds),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AC_CONFIG_MACRO_DIRS",
                M4MacroSignature {
                    arg_types: vec![
                        Paths(Blank, None), // dir1 [dir2 ... dirN]
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_CONFIG_AUX_DIR",
                M4MacroSignature {
                    arg_types: vec![
                        Path(None), // dir
                    ],
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        "ac_aux_dir_candidates".into(),
                        Var::reference("srcdir"),
                        Var::reference("PATH_SEPARATOR"),
                    ]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AC_CONFIG_AUX_DIR_DEFAULT",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        Var::reference("ac_aux_dir_candidates"),
                        "ac_aux_dir".into(),
                        "ac_install_sh".into(),
                        "ac_aux_dir_found".into(),
                        Var::reference("as_dir"),
                        Var::reference("ac_aux_dir"),
                        Var::reference("ac_install_sh"),
                    ]),
                    paths: Some(vec![
                        "install-sh".into(),
                        "install.sh".into(),
                        "shtool".into(),
                    ]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AC_REQUIRE_AUX_FILE",
                M4MacroSignature {
                    arg_types: vec![
                        Path(None), // file
                    ],
                    ret_type: Some(Cmds),
                    require: Some(vec!["AC_CONFIG_AUX_DIR_DEFAULT".into()]),
                    ..Default::default()
                },
            ),
            // Outputting files
            // the latest `AC_OUTPUT` does not take any arguments.
            // the below signature is actually an obsolete one.
            // but we can parse both with it.
            (
                "AC_OUTPUT",
                M4MacroSignature {
                    arg_types: vec![
                        Paths(Blank, Some(&split_tag)), // [file...]
                        Cmds,                           // [extra-cmds]
                        Cmds,                           // [init-cmds]
                    ],
                    ret_type: Some(Cmds),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AC_OUTPUT_COMMANDS", // obsolete macro, replaced by AC_CONFIG_COMMNDS
                M4MacroSignature {
                    replaced_by: Some("AC_CONFIG_COMMANDS".into()),
                    ..Default::default()
                },
            ),
            (
                "AC_PROG_MAKE_SET",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        Var::define_output("SET_MAKE").with_value(""),
                        Var::define_output("MAKE").with_value("make"),
                    ]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            // Creating configuration files
            (
                "AC_CONFIG_FILES",
                M4MacroSignature {
                    arg_types: vec![
                        Paths(Blank, Some(&split_tag)), // file...
                        Cmds,                           // [cmds]
                        Cmds,                           // [init-cmds]
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            // Configuration header files
            (
                "AC_CONFIG_HEADERS",
                M4MacroSignature {
                    arg_types: vec![
                        Paths(Blank, Some(&split_tag)), // header...
                        Cmds,                           // [cmds]
                        Cmds,                           // [init-cmds]
                    ],
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        // Set to '-DHAVE_CONFIG_H'
                        Var::define_output("DEFS").with_value("-DHAVE_CONFIG_H"),
                    ]),
                    cpp_symbols: Some(vec![CPP::set("HAVE_CONFIG_H")]),
                    ..Default::default()
                },
            ),
            (
                "AH_HEADER",
                M4MacroSignature {
                    ret_type: Some(Lit), // config header first declared by AC_CONFIG_HEADERS
                    ..Default::default()
                },
            ),
            (
                "AH_TEMPLATE",
                M4MacroSignature {
                    arg_types: vec![
                        CPPSymbol, // key
                        Lit,       // description
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            // while `AH_TEMPLATE` use the first argument(key) as a symbol name,
            // use of `AH_VERBATIM` don't indicate that the exact key is exported as a symbol.
            (
                "AH_VERBATIM",
                M4MacroSignature {
                    arg_types: vec![
                        Word, // key
                        Prog, // template
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AH_TOP",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // text
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AH_BOTTOM",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // text
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            // Running arbitrary configuration commands
            (
                "AC_CONFIG_COMMANDS",
                M4MacroSignature {
                    arg_types: vec![
                        Paths(Blank, None), // tag...
                        Cmds,               // [cmds]
                        Cmds,               // [init-cmds]
                    ],
                    ret_type: Some(Cmds),
                    paths: Some(vec!["config.status".into()]),
                    ..Default::default()
                },
            ),
            (
                "AH_CONFIG_COMMANDS_PRE",
                M4MacroSignature {
                    arg_types: vec![
                        Cmds, // cmds
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AH_CONFIG_COMMANDS_POST",
                M4MacroSignature {
                    arg_types: vec![
                        Cmds, // cmds
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            // Creating configuration links
            (
                "AC_CONFIG_LINKS",
                M4MacroSignature {
                    arg_types: vec![
                        Paths(
                            Blank,
                            Some(&|s| {
                                // 'DEST:SOURCE' -> [DEST, SOURCE]
                                split_tag(s)
                            }),
                        ),
                        Cmds, // [cmds]
                        Cmds, // [init-cmds]
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            // Configuring other packages in subdirectories
            (
                "AC_CONFIG_SUBDIRS",
                M4MacroSignature {
                    arg_types: vec![Paths(
                        Blank,
                        Some(&|s| {
                            // dir...
                            vec![
                                ExPath(format!("{}/configure", s)),
                                ExPath(format!("{}/configure.gnu", s)),
                            ]
                        }),
                    )],
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_output("subdirs")]),
                    ..Default::default()
                },
            ),
            // Default prefix
            (
                "AC_PREFIX_DEFAULT",
                M4MacroSignature {
                    arg_types: vec![
                        Path(None), // prefix
                    ],
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::reference("prefix"), "ac_default_prefix".into()]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AC_PREFIX_PROGRAM",
                M4MacroSignature {
                    arg_types: vec![
                        Word, // program (executable name)
                    ],
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        Var::reference_some("PATH", Environment),
                        Var::define_precious("prefix"),
                        Var::reference("ac_default_prefix"),
                        Var::reference("ac_prefix_program"),
                    ]),
                    ..Default::default()
                },
            ),
            // Default includes
            (
                "AC_INCLUDES_DEFAULT",
                M4MacroSignature {
                    arg_types: vec![
                        Prog, // [include-directives]
                    ],
                    ret_type: Some(Prog),
                    require: Some(vec!["AC_CHECK_INCLUDES_DEFAULT".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_CHECK_INCLUDES_DEFAULT",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        Var::define_internal("ac_includes_default").yes(),
                        Var::define_internal("ac_cv_header_stdlib_h").yes(),
                        Var::define_internal("ac_cv_header_string_h").yes(),
                    ]),
                    cpp_symbols: Some(vec![CPP::set("STDC_HEADERS")]),
                    paths: Some(vec![
                        "stdio.h".into(),
                        "stdlib.h".into(),
                        "string.h".into(),
                        "inttypes.h".into(),
                        "stdint.h".into(),
                        "strings.h".into(),
                        "sys/types.h".into(),
                        "sys/stat.h".into(),
                        "unistd.h".into(),
                    ]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            // paticular program checks
            (
                "AC_PROG_AR",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        Var::define_output("AR").with_value("ar"),
                        Var::define_internal("ac_cv_prog_ar").with_value("ar"),
                    ]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AC_PROG_AWK",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        Var::define_output("AWK").with_value("awk"),
                        Var::define_internal("ac_cv_prog_AWK").with_value("awk"),
                    ]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AC_PROG_GREP",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        Var::define_output("GREP").with_value("grep"),
                        Var::define_internal("ac_cv_path_GREP").with_value("grep"),
                    ]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AC_PROG_EGREP",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        Var::define_output("EGREP").with_value("egrep"),
                        Var::define_internal("ac_cv_path_EGREP").with_value("egrep"),
                    ]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AC_PROG_FGREP",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        Var::define_output("FGREP").with_value("fgrep"),
                        Var::define_internal("ac_cv_path_FGREP").with_value("fgrep"),
                    ]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AC_PROG_INSTALL",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        Var::define_output("INSTALL").with_value("install"),
                        Var::define_output("INSTALL_PROGRAM").with_value("install"),
                        Var::define_output("INSTALL_SCRIPT").with_value("install"),
                        Var::define_output("INSTALL_DATA").with_value("install -m 644"),
                        Var::define_internal("ac_cv_path_install").with_value("install"),
                    ]),
                    require: Some(vec![
                        // _AC_INIT_AUX_DIR is an internal macro required by
                        // AC_REQUIRE_AUX_FILE that is called by AC_PROG_INSTALL.
                        // Instead we require an equivalent macro: AC_CONFIG_AUX_DIR_DEFAULT.
                        "AC_CONFIG_AUX_DIR_DEFAULT".into(),
                    ]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AC_PROG_MKDIR_P",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        Var::define_output("MKDIR_P").with_value("mkdir -p"),
                        Var::define_internal("ac_cv_path_mkdir").with_value("mkdir -p"),
                    ]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AC_PROG_LEX",
                M4MacroSignature {
                    arg_types: vec![Arr(Blank)],
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        Var::define_output("LEX").with_value("flex"),
                        Var::define_output("LEX_OUTPUT_ROOT").with_value("lex.yy"),
                        Var::define_output("LEXLIB").with_value(""),
                        Var::define_internal("ac_cv_prog_LEX").with_value("flex"),
                    ]),
                    is_oneshot: true,
                    cpp_symbols: Some(vec![CPP::set("YYTEXT_POINTER")]),
                    ..Default::default()
                },
            ),
            (
                "AC_PROG_LN_S",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_output("LN_S").with_value("ln -s")]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AC_PROG_RANLIB",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        Var::define_output("RANLIB").with_value("ranlib"),
                        Var::define_internal("ac_cv_prog_ranlib").with_value("ranlib"),
                    ]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AC_PROG_SED",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        Var::define_output("SED").with_value("sed"),
                        Var::define_internal("ac_cv_path_SED").with_value("sed"),
                    ]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AC_PROG_YACC",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        Var::define_precious("YACC").with_value("yacc"),
                        Var::define_precious("YFLAGS").with_value(""),
                        Var::define_internal("ac_cv_prog_YACC").with_value("yacc"),
                    ]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            // Automake macros
            // the first argument of `AM_CONDITIONAL` denotes a variable
            // to be used in Makefile.am, not in shell
            (
                "AM_CONDITIONAL",
                M4MacroSignature {
                    arg_types: vec![
                        VarName(None, Some(&|s| vec![ExCond(s.into())])), // conditional (definition of AM var)
                        AMCond,                                           // condition
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AM_COND_IF",
                M4MacroSignature {
                    arg_types: vec![
                        Lit,  // conditional (reference to AM var)
                        Cmds, // [if-true]
                        Cmds, // [if-false]
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AM_PROG_AR",
                M4MacroSignature {
                    arg_types: vec![
                        Cmds, // [act-if-fail]
                    ],
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_output("AR").with_value("ar")]),
                    paths: Some(vec!["ar-lib".into()]), // prefix is $ac_aux_dir
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AM_PROG_AS",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        Var::define_precious("CCAS").with_value("cc -c"),
                        Var::define_precious("CCASFLAGS").with_value(""),
                    ]),
                    require: Some(vec!["AC_PROG_CC".into()]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AM_PROG_CC_C_O",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    paths: Some(vec![
                        // by AC_REQUIRE_AUX_FILE([compile])
                        "compile".into(), // prefix is $ac_aux_dir
                    ]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AM_PROG_MKDIR_P",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_output("mkdir_p").with_value("mkdir -p")]),
                    require: Some(vec!["AC_PROG_MKDIR_P".into()]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AM_PROG_LEX",
                M4MacroSignature {
                    arg_types: vec![Arr(Blank)],
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        // the differece from AC_PROG_LEX is the LEX's value when lex is not found.
                        // missing script will be triggered instead.
                        Var::define_output("LEX").with_value("flex"),
                        Var::define_output("LEX_OUTPUT_ROOT").with_value("lex.yy"),
                        Var::define_output("LEXLIB").with_value(""),
                        Var::define_internal("ac_cv_prog_LEX").with_value("flex"),
                    ]),
                    cpp_symbols: Some(vec![CPP::set("YYTEXT_POINTER")]),
                    paths: Some(vec![
                        // by AC_REQUIRE_AUX_FILE([missing])
                        "missing".into(), // prefix is $ac_aux_dir
                    ]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AM_PROG_GCJ",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        Var::define_output("GCJ"),
                        Var::define_output("GCJFLAGS"),
                        Var::define_internal("ac_cv_prog_GCJ"),
                        // by _AM_DEPENDENCIES([GCJ])
                        Var::define_output("GCJDEPMODE"),
                    ]),
                    am_conds: Some(vec![
                        AmCond::new("am__fastdepGCJ").set(),
                    ]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AM_PROG_UPC",
                M4MacroSignature {
                    arg_types: vec![Arr(Blank)],
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        Var::define_precious("UPC"),
                        Var::define_precious("UPCFLAGS"),
                        Var::define_output("GCJFLAGS"),
                        "ac_cv_prog_UPC".into(),
                        // by _AM_DEPENDENCIES([UPC])
                        Var::define_output("UPCDEPMODE"),
                    ]),
                    am_conds: Some(vec![
                        AmCond::new("am__fastdepUPC").set(),
                    ]),
                    require: Some(vec!["AC_PROG_CC".into()]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AM_MISSING_PROG",
                M4MacroSignature {
                    arg_types: vec![
                        VarName(Some(VarAttrs::output()), None), // name
                        Word,                                    // program
                        Path(None),                              // [missing_dir] obsolete argument
                    ],
                    ret_type: Some(Cmds),
                    paths: Some(vec![
                        // by AC_REQUIRE_AUX_FILE([missing])
                        "missing".into(), // prefix is $ac_aux_dir
                    ]),
                    ..Default::default()
                },
            ),
            (
                "AM_SILENT_RULES",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // yes
                    ],
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        // by _AM_SILENT_RULES
                        Var::define_input("enable_silent_rules"),
                        Var::define_output("AM_V").with_value("0"),
                        Var::define_output("AM_DEFAULT_V").with_value("0"),
                        Var::define_output("AM_DEFAULT_VERBOSITY").with_value("0"),
                        Var::define_output("AM_BACKSLASH").with_value("\\"),
                        Var::define_output("AM_V_at").with_value(""),
                        Var::define_output("AM_V_GEN").with_value(""),
                    ]),
                    ..Default::default()
                },
            ),
            (
                "AM_WITH_DMALLOC",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        // by _AM_SILENT_RULES
                        Var::define_input("with_dmalloc").yes(),
                        // '-ldmalloc' is added to LIBS
                        Var::append("LIBS", Output).with_value("-ldmalloc"),
                        // '-g' is added to LDFLAGS
                        Var::append("LDFLAGS", Output).with_value("-g"),
                    ]),
                    cpp_symbols: Some(vec![CPP::set("WITH_DMALLOC")]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AM_PATH_LISPDIR",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        Var::define_output("lispdir"),
                        Var::define_precious("EMACS"),
                    ]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AM_PATH_PYTHON",
                M4MacroSignature {
                    arg_types: vec![
                        Word, // [version]
                        Cmds, // [action-if-found]
                        Cmds, // [action-if-not-found]
                    ],
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        Var::define_output("PYTHON"),
                        Var::define_output("PYTHON_VERSION"),
                        Var::define_output("PYTHON_PREFIX"),
                        Var::define_output("PYTHON_EXEC_PREFIX"),
                        Var::define_output("PYTHON_PLATFORM"),
                        Var::define_output("pythondir"),
                        Var::define_output("pkgpythondir"),
                        Var::define_output("pyexecdir"),
                        Var::define_output("pkgpyexecdir"),
                    ]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AM_MAINTAINER_MODE",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // [enabled]
                    ],
                    ret_type: Some(Cmds),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            // generic program and file checks
            (
                "AC_CHECK_PROG",
                M4MacroSignature {
                    arg_types: vec![
                        VarName(
                            Some(VarAttrs::output()),
                            Some(&|s| {
                                // variable
                                vec![ExVar(
                                    VarAttrs::internal(),
                                    format!("ac_cv_prog_{}", s),
                                    None,
                                )]
                            }),
                        ),
                        Word,       // prog-to-check-for
                        Word,       // value-if-found
                        Word,       // [value-if-not-found]
                        Path(None), // [path=$PATH]
                        Path(None), // [REJECT]
                    ],
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::reference_some("PATH", Environment)]),
                    ..Default::default()
                },
            ),
            (
                "AC_CHECK_PROGS",
                M4MacroSignature {
                    arg_types: vec![
                        VarName(
                            Some(VarAttrs::output()),
                            Some(&|s| {
                                // variable
                                vec![ExVar(
                                    VarAttrs::internal(),
                                    format!("ac_cv_prog_{}", s),
                                    None,
                                )]
                            }),
                        ),
                        Arr(Blank), // progs-to-check-for
                        Word,       // [value-if-not-found]
                        Path(None), // [path=$PATH]
                    ],
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::reference_some("PATH", Environment)]),
                    ..Default::default()
                },
            ),
            (
                "AC_CHECK_TOOL",
                M4MacroSignature {
                    arg_types: vec![
                        VarName(
                            Some(VarAttrs::output()),
                            Some(&|s| {
                                // variable
                                vec![
                                    ExVar(VarAttrs::internal(), format!("ac_cv_prog_{}", s), None),
                                    // _AC_CHECK_PROG([ac_ct_$1], ...)
                                    ExVar(VarAttrs::output(), format!("ac_ct_{}", s), None),
                                    ExVar(
                                        VarAttrs::internal(),
                                        format!("ac_cv_prog_ac_ct_{}", s),
                                        None,
                                    ),
                                ]
                            }),
                        ),
                        Word,       // prog-to-check-for
                        Word,       // [value-if-not-found]
                        Path(None), // [path=$PATH]
                    ],
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        Var::reference_some("PATH", Environment),
                        Var::reference("host_alias"),
                        Var::reference("ac_tool_prefix"),
                    ]),
                    ..Default::default()
                },
            ),
            (
                "AC_CHECK_TOOLS",
                M4MacroSignature {
                    arg_types: vec![
                        VarName(
                            Some(VarAttrs::output()),
                            Some(&|s| {
                                // variable
                                vec![
                                    ExVar(VarAttrs::internal(), format!("ac_cv_prog_{}", s), None),
                                    // _AC_CHECK_PROG([ac_ct_$1], ...)
                                    ExVar(VarAttrs::output(), format!("ac_ct_{}", s), None),
                                    ExVar(
                                        VarAttrs::internal(),
                                        format!("ac_cv_prog_ac_ct_{}", s),
                                        None,
                                    ),
                                ]
                            }),
                        ),
                        Arr(Blank), // progs-to-check-for
                        Word,       // [value-if-not-found]
                        Path(None), // [path=$PATH]
                    ],
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        Var::reference_some("PATH", Environment),
                        Var::reference("host_alias"),
                        Var::reference("ac_tool_prefix"),
                    ]),
                    ..Default::default()
                },
            ),
            (
                "AC_CHECK_TARGET_TOOL",
                M4MacroSignature {
                    arg_types: vec![
                        VarName(
                            Some(VarAttrs::output()),
                            Some(&|s| {
                                // variable
                                vec![ExVar(
                                    VarAttrs::internal(),
                                    format!("ac_cv_prog_{}", s),
                                    None,
                                )]
                            }),
                        ),
                        Word,       // prog-to-check-for
                        Word,       // [value-if-not-found]
                        Path(None), // [path=$PATH]
                    ],
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        Var::reference_some("PATH", Environment),
                        Var::reference("target_alias"),
                        Var::reference("target"),
                        Var::reference("build"),
                    ]),
                    ..Default::default()
                },
            ),
            (
                "AC_CHECK_TARGET_TOOLS",
                M4MacroSignature {
                    arg_types: vec![
                        VarName(
                            Some(VarAttrs::output()),
                            Some(&|s| {
                                // variable
                                vec![ExVar(
                                    VarAttrs::internal(),
                                    format!("ac_cv_prog_{}", s),
                                    None,
                                )]
                            }),
                        ),
                        Arr(Blank), // progs-to-check-for
                        Word,       // [value-if-not-found]
                        Path(None), // [path=$PATH]
                    ],
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        Var::reference_some("PATH", Environment),
                        Var::reference("target_alias"),
                        Var::reference("target"),
                        Var::reference("build"),
                    ]),
                    ..Default::default()
                },
            ),
            (
                "AC_PATH_PROG",
                M4MacroSignature {
                    arg_types: vec![
                        VarName(
                            Some(VarAttrs::output()),
                            Some(&|s| {
                                // variable
                                vec![ExVar(
                                    VarAttrs::internal(),
                                    format!("ac_cv_path_{}", s),
                                    None,
                                )]
                            }),
                        ),
                        Arr(Blank), // prog-to-check-for
                        Word,       // [value-if-not-found]
                        Path(None), // [path=$PATH]
                    ],
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::reference("PATH")]),
                    ..Default::default()
                },
            ),
            (
                "AC_PATH_PROGS",
                M4MacroSignature {
                    arg_types: vec![
                        VarName(
                            Some(VarAttrs::output()),
                            Some(&|s| {
                                // variable
                                vec![
                                    ExVar(VarAttrs::internal(), format!("ac_cv_path_{}", s), None),
                                    ExVar(VarAttrs::internal(), format!("ac_cv_path_{}", s), None),
                                ]
                            }),
                        ),
                        Arr(Blank), // progs-to-check-for
                        Word,       // [value-if-not-found]
                        Path(None), // [path=$PATH]
                    ],
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::reference_some("PATH", Environment)]),
                    ..Default::default()
                },
            ),
            (
                "AC_PATH_PROGS_FEATURE_CHECK",
                M4MacroSignature {
                    arg_types: vec![
                        // FIXME: it is Var but not exported. only updates the cache variable.
                        VarName(
                            Some(VarAttrs::output()),
                            Some(&|s| {
                                // variable
                                vec![ExVar(
                                    VarAttrs::internal(),
                                    format!("ac_cv_path_{}", s),
                                    None,
                                )]
                            }),
                        ),
                        Arr(Blank), // progs-to-check-for
                        Cmds,       // feature-test
                        Cmds,       // [action-if-not-found]
                        Path(None), // [path=$PATH]
                    ],
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::reference_some("PATH", Environment)]),
                    ..Default::default()
                },
            ),
            (
                "AC_PATH_TOOL",
                M4MacroSignature {
                    arg_types: vec![
                        VarName(
                            Some(VarAttrs::output()),
                            Some(&|s| {
                                // variable
                                vec![ExVar(
                                    VarAttrs::internal(),
                                    format!("ac_cv_prog_{}", s),
                                    None,
                                )]
                            }),
                        ),
                        Word,
                        Word,
                        Path(None),
                    ],
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        Var::reference_some("PATH", Environment),
                        Var::reference("target_alias"),
                        Var::reference("target"),
                        Var::reference("build"),
                    ]),
                    ..Default::default()
                },
            ),
            (
                "AC_PATH_TARGET_TOOL",
                M4MacroSignature {
                    arg_types: vec![
                        VarName(
                            Some(VarAttrs::output()),
                            Some(&|s| {
                                //variable
                                vec![ExVar(
                                    VarAttrs::internal(),
                                    format!("ac_cv_prog_{}", s),
                                    None,
                                )]
                            }),
                        ),
                        Word,       // prog-to-check-for
                        Cmds,       // [value-if-not-found]
                        Path(None), // [path=$PATH]
                    ],
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        Var::reference_some("PATH", Environment),
                        Var::reference("target_alias"),
                        Var::reference("target"),
                        Var::reference("build"),
                    ]),
                    ..Default::default()
                },
            ),
            // Files
            (
                "AC_CHECK_FILE",
                M4MacroSignature {
                    arg_types: vec![
                        Path(Some(&|s| {
                            // file
                            vec![ExVar(
                                VarAttrs::new(Internal, Defined),
                                format!("ac_cv_file_{}", sanitize_shell_name(s)),
                                None,
                            )]
                        })),
                        Cmds, // [action-if-found]
                        Cmds, // [action-if-not-found]
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_CHECK_FILES",
                M4MacroSignature {
                    arg_types: vec![
                        Paths(
                            Blank,
                            Some(&|s| {
                                // files
                                vec![
                                    ExVar(
                                        VarAttrs::internal(),
                                        format!("ac_cv_file_{}", sanitize_shell_name(s)),
                                        None,
                                    ),
                                    ExCPP(format!("HAVE_{}", sanitize_c_name(s)), None),
                                ]
                            }),
                        ),
                        Cmds, // [action-if-found]
                        Cmds, // [action-if-not-found]
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            // Library files
            (
                "AC_CHECK_LIB",
                M4MacroSignature {
                    arg_types: vec![
                        Library(Some(&|s| {
                            // library
                            vec![
                                // FIXME: actually this macro exports ac_cv_lib_{LIBRARY}_{FUNCTION}.
                                // but to define it needs to captre two arguments, which is not supported now.
                                ExCPP(format!("HAVE_LIB{}", sanitize_c_name(s)), None),
                            ]
                        })),
                        Symbol(None), // function
                        Cmds,         // [action-if-found]
                        Cmds,         // [action-if-not-found]
                        Arr(Blank),   // [other-libraries]
                    ],
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        Var::reference("LDFLAGS"),
                        // '-l<library>' is added
                        Var::append("LIBS", Output),
                    ]),
                    ..Default::default()
                },
            ),
            (
                "AC_HAVE_LIBRARY", // obsolete. equivalent to AC_CHECK_LIB(..., main, ...)
                M4MacroSignature {
                    arg_types: vec![
                        Library(Some(&|s| {
                            // library
                            vec![ExCPP(format!("HAVE_LIB{}", sanitize_c_name(s)), None)]
                        })),
                        Cmds,       // [action-if-found]
                        Cmds,       // [action-if-not-found]
                        Arr(Blank), // [other-libraries]
                    ],
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        // '-l<library>' is added
                        Var::append("LIBS", Output),
                    ]),
                    ..Default::default()
                },
            ),
            (
                "AC_SEARCH_LIBS",
                M4MacroSignature {
                    arg_types: vec![
                        Symbol(Some(&|s| {
                            // function
                            vec![ExVar(
                                VarAttrs::internal(),
                                format!("ac_cv_search_{}", s),
                                None,
                            )]
                        })),
                        Arr(Blank), // search-libs
                        Cmds,       // [action-if-found]
                        Cmds,       // [action-if-not-found]
                        Arr(Blank), // [other-libraries]
                    ],
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        // '-l<library>' is added
                        Var::append("LIBS", Output),
                    ]),
                    ..Default::default()
                },
            ),
            // Paticular function checks
            (
                // FIXME: we intentionally fixed this macro pretending like there is no support for 'alloca'.
                // Actually modern C ecosystem must have it, but sorry i did this just for my personal project for now.
                "AC_FUNC_ALLOCA",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_output("ALLOCA").with_value("")]),
                    cpp_symbols: Some(vec![
                        CPP::unset("HAVE_ALLOC_H"),
                        CPP::unset("HAVE_ALLOCA"),
                        CPP::unset("C_ALLOCA"),
                    ]),
                    ..Default::default()
                },
            ),
            (
                "AC_ALLOCA", // obsolete. replaced by AC_FUNC_ALLOCA
                M4MacroSignature {
                    replaced_by: Some("AC_FUNC_ALLOCA".into()),
                    ..Default::default()
                },
            ),
            (
                "AC_FUNC_CHOWN",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_internal("ac_cv_func_chown_works").yes()]),
                    cpp_symbols: Some(vec![CPP::set("HAVE_CHOWN")]),
                    ..Default::default()
                },
            ),
            (
                "AC_FUNC_CLOSEDIR_VOID",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_internal("ac_cv_func_closedir_void").no()]),
                    cpp_symbols: Some(vec![CPP::unset("CLOSEDIR_VOID")]),
                    ..Default::default()
                },
            ),
            (
                "AC_DIR_HEADER", // obsolete.
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        // AC_HEADER_DIRENT
                        Var::define_internal("ac_header_dirent"),
                        Var::define_internal("ac_cv_search_opendir").with_value("none required"),
                        // AC_FUNC_CLOSEDIR_VOID
                        Var::define_internal("ac_cv_func_closedir_void").no(),
                    ]),
                    cpp_symbols: Some(vec![
                        // AC_HEADER_DIRENT
                        CPP::set("HAVE_DIRENT_H"),
                        CPP::unset("HAVE_SYS_NDIR_H"),
                        CPP::unset("HAVE_SYS_DIR_H"),
                        CPP::unset("HAVE_NDIR_H"),
                        // AC_FUNC_CLOSEDIR_VOID
                        CPP::unset("CLOSEDIR_VOID"),
                        // old symbols
                        CPP::set("DIRENT"),
                        CPP::unset("SYSNDIR"),
                        CPP::unset("SYSDIR"),
                        CPP::unset("NDIR"),
                    ]),
                    paths: Some(vec![
                        // AC_HEADER_DIRENT
                        "dirent.h".into(),
                        "sys/ndir.h".into(),
                        "sys/dir.h".into(),
                        "ndir.h".into(),
                    ]),

                    ..Default::default()
                },
            ),
            (
                "AC_FUNC_ERROR_AT_LINE",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_internal("ac_cv_lib_error_at_line").yes()]),
                    paths: Some(vec!["path.c".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_FUNC_FNMATCH",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_internal("ac_cv_func_fnmatch_works").yes()]),
                    cpp_symbols: Some(vec![CPP::set("HAVE_FNMATCH")]),
                    ..Default::default()
                },
            ),
            (
                "AC_FUNC_FNMATCH_GNU",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_internal("ac_cv_func_fnmatch_gnu").no()]),
                    cpp_symbols: Some(vec![CPP::set("HAVE_FNMATCH")]),
                    ..Default::default()
                },
            ),
            (
                "AC_FUNC_FORK",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        Var::define_internal("ac_cv_func_fork_works").yes(),
                        Var::define_internal("ac_cv_func_vfork_works").yes(),
                        Var::define_internal("ac_cv_func_fork").yes(),
                        Var::define_internal("ac_cv_func_vfork").yes(),
                    ]),
                    cpp_symbols: Some(vec![
                        CPP::unset("HAVE_WORKING_FORK"),
                        CPP::set("HAVE_VFORK"),
                    ]),
                    ..Default::default()
                },
            ),
            (
                "AC_FUNC_FSEEKO",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    cpp_symbols: Some(vec![CPP::set("HAVE_FSEEKO"), CPP::set("_LARGEFILE_SOURCE")]),
                    ..Default::default()
                },
            ),
            (
                "AC_FUNC_GETGROUPS",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_output("GETGROUPS_LIB")]),
                    cpp_symbols: Some(vec![CPP::set("HAVE_GETGROUPS")]),
                    ..Default::default()
                },
            ),
            (
                "AC_FUNC_GETLOADAVG",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        Var::define_output("GETLOADAVG_LIBS").with_value(""),
                        // '-l<lib>' for lib in GETLOADAVG_LIBS
                        Var::append("LIBS", Output).with_value(""),
                        // if the system does not have the 'getloadavg' function.
                        Var::define_output("NEED_SETGID").with_value("false"),
                        Var::define_output("KMEM_GROUP").with_value(""),
                    ]),
                    cpp_symbols: Some(vec![
                        CPP::set("HAVE_GETLOADAVG"),
                        // if the system does not have the 'getloadavg' function.
                        CPP::unset("C_GETLOADAVG"),
                        CPP::unset("SVR4"),
                        CPP::unset("DGUX"),
                        CPP::unset("UMAX"),
                        CPP::unset("UMAX4_3"),
                        CPP::set("HAVE_NLIST_H"),
                        CPP::set("HAVE_STRUCT_NLIST_N_UN_N_NAME"),
                        CPP::unset("GETLOADAVG_PREVILEGED"),
                    ]),
                    paths: Some(vec!["getloadavg.c".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_GETLOADAVG", // obsolete. replaced by AC_FUNC_GETLOADAVG
                M4MacroSignature {
                    replaced_by: Some("AC_FUNC_GETLOADAVG".into()),
                    ..Default::default()
                },
            ),
            (
                "AC_FUNC_GETMNTENT",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        Var::define_internal("ac_cv_func_getmntent").yes(),
                        Var::define_internal("ac_cv_search_getmntent").yes(),
                    ]),
                    cpp_symbols: Some(vec![CPP::set("HAVE_SETMNTENT")]),
                    ..Default::default()
                },
            ),
            (
                "AC_DYNIX_SEQ", // obsolete. aliased to AC_FUNC_GETMNTENT
                M4MacroSignature {
                    replaced_by: Some("AC_FUNC_GETMNTENT".into()),
                    ..Default::default()
                },
            ),
            (
                "AC_OBJEXT", // obsolete. now done automaticallly.
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_output("OBJEXT").with_value("o")]),
                    ..Default::default()
                },
            ),
            (
                "AC_EXEEXT", // obsolete. now done automaticallly.
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_output("EXEEXT").with_value("")]),
                    ..Default::default()
                },
            ),
            (
                "AC_FUNC_GETPGRP",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_internal("ac_cv_func_getpgrp_void").yes()]),
                    cpp_symbols: Some(vec![CPP::set("GETPGRP_VOID")]),
                    ..Default::default()
                },
            ),
            (
                "AC_FUNC_LSTAT_FOLLOWS_SLASHED_SYMLINK",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_internal(
                        "ac_cv_func_lstat_dereferences_slashed_symlink",
                    )
                    .yes()]),
                    cpp_symbols: Some(vec![CPP::set("LSTAT_FOLLOWS_SYMLINK")]),
                    paths: Some(vec!["lstat.c".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_FUNC_MALLOC",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        Var::define_internal("ac_cv_func_malloc_0_nonnull").yes()
                    ]),
                    cpp_symbols: Some(vec![
                        CPP::set("HAVE_MALLOC"),
                        // if the 'macro' function is not compatible to GNU C
                        CPP::unset("malloc"), // overridden by 'rpl_malloc'
                    ]),
                    paths: Some(vec!["malloc.c".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_FUNC_MBRTOWC",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_internal("ac_cv_func_mbrtowc").yes()]),
                    cpp_symbols: Some(vec![CPP::set("HAVE_MBRTOWC")]),
                    ..Default::default()
                },
            ),
            (
                "AC_FUNC_MEMCMP",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_internal("ac_cv_func_memcmp_working").yes()]),
                    paths: Some(vec!["memcmp.c".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_FUNC_MKTIME",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_internal("ac_cv_func_working_mktime").yes()]),
                    paths: Some(vec!["mktime.c".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_FUNC_MMAP",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        Var::define_internal("ac_cv_func_mmap_fixed_mapped").yes()
                    ]),
                    cpp_symbols: Some(vec![CPP::set("HAVE_MMAP")]),
                    ..Default::default()
                },
            ),
            (
                "AC_FUNC_OBSTACK",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_internal("ac_cv_func_obstack").yes()]),
                    cpp_symbols: Some(vec![CPP::set("HAVE_OBSTACK")]),
                    paths: Some(vec!["obstack.c".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_FUNC_REALLOC",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        Var::define_internal("ac_cv_func_realloc_0_nonnull").yes()
                    ]),
                    cpp_symbols: Some(vec![CPP::set("HAVE_REALLOC"), CPP::unset("realloc")]),
                    paths: Some(vec!["realloc.c".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_FUNC_SELECT_ARGTYPES",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    cpp_symbols: Some(vec![
                        CPP::with_value("SELECT_TYPE_ARG1", "int"),
                        CPP::with_value("SELECT_TYPE_ARG234", "(fd_set *)"),
                        CPP::with_value("SELECT_TYPE_ARG5", "(struct timeval *)"),
                    ]),
                    ..Default::default()
                },
            ),
            (
                "AC_FUNC_SETPGRP",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_internal("ac_cv_func_setpgrp_void").yes()]),
                    cpp_symbols: Some(vec![CPP::set("SETPGRP_VOID")]),
                    ..Default::default()
                },
            ),
            (
                "AC_FUNC_STAT",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_internal(
                        "ac_cv_func_stat_empty_string_bug",
                    )
                    .no()]),
                    cpp_symbols: Some(vec![CPP::unset("HAVE_STAT_EMPTY_STRING_BUG")]),
                    paths: Some(vec!["stat.c".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_FUNC_LSTAT",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_internal(
                        "ac_cv_func_lstat_empty_string_bug",
                    )
                    .no()]),
                    cpp_symbols: Some(vec![CPP::unset("HAVE_LSTAT_EMPTY_STRING_BUG")]),
                    paths: Some(vec!["lstat.c".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_FUNC_STRCOLL",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_internal(
                        "ac_cv_func_lstat_empty_string_bug",
                    )
                    .no()]),
                    cpp_symbols: Some(vec![CPP::unset("HAVE_LSTAT_EMPTY_STRING_BUG")]),
                    ..Default::default()
                },
            ),
            (
                "AC_FUNC_STRERROR_R",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        Var::define_internal("ac_cv_func_stderror_r_char_p").yes()
                    ]),
                    cpp_symbols: Some(vec![CPP::set("HAVE_DECL_STDERROR_R")]),
                    ..Default::default()
                },
            ),
            (
                "AC_FUNC_STRFTIME",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    cpp_symbols: Some(vec![CPP::set("HAVE_STRFTIME")]),
                    ..Default::default()
                },
            ),
            (
                "AC_FUNC_STRTOD",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        Var::define_output("POW_LIB").with_value(""),
                        Var::define_internal("ac_cv_func_strtod").yes(),
                        Var::define_internal("ac_cv_func_pow").yes(),
                    ]),
                    paths: Some(vec!["strtod.c".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_FUNC_STRTOLD",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_internal("ac_cv_func_strtold").yes()]),
                    cpp_symbols: Some(vec![CPP::set("HAVE_STRTOLD")]),
                    ..Default::default()
                },
            ),
            (
                "AC_FUNC_STRNLEN",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        Var::define_internal("ac_cv_func_strnlen_working").yes()
                    ]),
                    cpp_symbols: Some(vec![CPP::set("HAVE_STRTOLD")]),
                    paths: Some(vec!["strnlen.c".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_FUNC_UTIME_NULL",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_internal("ac_cv_func_utime_null").yes()]),
                    cpp_symbols: Some(vec![CPP::set("HAVE_UTIME_NULL")]),
                    ..Default::default()
                },
            ),
            (
                "AC_FUNC_VPRINTF",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    cpp_symbols: Some(vec![
                        CPP::set("HAVE_VPRINTF"),
                        // otherwise if '_doprnt' if found
                        CPP::unset("HAVE_DOPRNT"),
                    ]),
                    ..Default::default()
                },
            ),
            (
                "AC_REPLACE_FNMATCH",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_internal("ac_cv_func_fnmatch_works").yes()]),
                    paths: Some(vec![
                        "fnmatch.c".into(),
                        "fnmatch_loop.c".into(),
                        "fnmatch.h".into(),
                        "fnmatch_.h".into(),
                    ]),
                    ..Default::default()
                },
            ), // Generic function checks
            (
                "AC_CHECK_FUNC",
                M4MacroSignature {
                    arg_types: vec![
                        Symbol(Some(&|s| {
                            // function
                            vec![ExVar(
                                VarAttrs::internal(),
                                format!("ac_cv_func_{}", s),
                                None,
                            )]
                        })),
                        Cmds, // [action-if-found]
                        Cmds, // [action-if-not-found]
                    ],
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        // the macro uses the flags
                        Var::reference("LDFLAGS"),
                        Var::reference("LIBS"),
                    ]),
                    ..Default::default()
                },
            ),
            (
                "AC_FUNC_CHECK", // obsolete. aliased to AC_CHECK_FUNC
                M4MacroSignature {
                    replaced_by: Some("AC_CHECK_FUNC".into()),
                    ..Default::default()
                },
            ),
            (
                "AC_CHECK_FUNCS",
                M4MacroSignature {
                    arg_types: vec![
                        Symbols(
                            Blank,
                            Some(&|s| {
                                // function
                                vec![
                                    ExVar(VarAttrs::internal(), format!("ac_cv_func_{}", s), None),
                                    ExCPP(format!("HAVE_{}", sanitize_c_name(s)), None),
                                ]
                            }),
                        ),
                        Cmds, // [action-if-found]
                        Cmds, // [action-if-not-found]
                    ],
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        // the macro uses the flags
                        Var::reference("LDFLAGS"),
                        Var::reference("LIBS"),
                    ]),
                    ..Default::default()
                },
            ),
            (
                "AC_HAVE_FUNCS", // obsolete. aliased to AC_CHECK_FUNCS
                M4MacroSignature {
                    replaced_by: Some("AC_CHECK_FUNCS".into()),
                    ..Default::default()
                },
            ),
            (
                "AC_CHECK_FUNCS_ONCE",
                M4MacroSignature {
                    arg_types: vec![Symbols(
                        Blank,
                        Some(&|s| {
                            // function...
                            vec![
                                ExVar(VarAttrs::internal(), format!("ac_cv_func_{}", s), None),
                                ExCPP(format!("HAVE_{}", sanitize_c_name(s)), None),
                            ]
                        }),
                    )],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_LIBOBJ",
                M4MacroSignature {
                    arg_types: vec![Symbol(Some(&|s| {
                        // function
                        vec![ExPath(format!("{}.c", s))]
                    }))],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_LIBSOURCE",
                M4MacroSignature {
                    arg_types: vec![
                        Path(None), // file
                    ],
                    ret_type: None,
                    ..Default::default()
                },
            ),
            (
                "AC_LIBSOURCES",
                M4MacroSignature {
                    arg_types: vec![
                        Paths(Comma, None), // files
                    ],
                    ret_type: None,
                    ..Default::default()
                },
            ),
            (
                "AC_CONFIG_LIBOBJ_DIR",
                M4MacroSignature {
                    arg_types: vec![
                        Path(None), // directory
                    ],
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec!["ac_config_libobj_dir".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_REPLACE_FUNCS",
                M4MacroSignature {
                    arg_types: vec![Symbols(
                        Blank,
                        Some(&|s| {
                            // function...
                            vec![
                                ExVar(VarAttrs::internal(), format!("ac_cv_func_{}", s), None),
                                ExCPP(format!("HAVE_{}", sanitize_c_name(s)), None),
                                ExPath(format!("{}.c", s)),
                            ]
                        }),
                    )],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            // Paticular header checks
            (
                "AC_CHECK_HEADER_STDBOOL",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_internal("ac_cv_header_stdbool_h").yes()]),
                    cpp_symbols: Some(vec![CPP::set("HAVE__BOOL")]),
                    paths: Some(vec!["stdbool.h".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_HEADER_ASSERT",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        // internally `AC_ARG_ENABLE(assert,..)` is called
                        Var::define_input("ac_enable_assert").yes(),
                    ]),
                    cpp_symbols: Some(vec![CPP::set("NDEBUG")]),
                    ..Default::default()
                },
            ),
            (
                "AC_HEADER_DIRENT",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        Var::define_internal("ac_header_dirent").with_value("dirent.h"),
                        Var::define_internal("ac_cv_search_opendir").yes(),
                    ]),
                    cpp_symbols: Some(vec![
                        CPP::set("HAVE_DIRENT_H"),
                        CPP::set("HAVE_SYS_NDIR_H"),
                        CPP::set("HAVE_SYS_DIR_H"),
                        CPP::set("HAVE_NDIR_H"),
                    ]),
                    paths: Some(vec![
                        "dirent.h".into(),
                        "sys/ndir.h".into(),
                        "sys/dir.h".into(),
                        "ndir.h".into(),
                    ]),
                    ..Default::default()
                },
            ),
            (
                "AC_HEADER_MAJOR",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_internal("ac_cv_header_sys_mkdev_h").no()]),
                    cpp_symbols: Some(vec![
                        CPP::unset("MAJOR_IN_MKDEV"),
                        CPP::set("MAJOR_IN_SYSMACROS"),
                    ]),
                    paths: Some(vec![
                        "sys/mkdev.h".into(),
                        "sys/sysmacros.h".into(),
                        "sys/types.h".into(),
                    ]),
                    ..Default::default()
                },
            ),
            (
                "AC_HEADER_RESOLV",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_internal("ac_cv_header_sys_mkdev_h").no()]),
                    cpp_symbols: Some(vec![
                        CPP::unset("MAJOR_IN_MKDEV"),
                        CPP::set("MAJOR_IN_SYSMACROS"),
                    ]),
                    paths: Some(vec![
                        "sys/types.h".into(),
                        "netinet/in.h".into(),
                        "arpa/nameser.h".into(),
                        "netdb.h".into(),
                        "resolv.h".into(),
                    ]),
                    ..Default::default()
                },
            ),
            (
                "AC_HEADER_STAT",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_internal("ac_cv_header_stat_broken").no()]),
                    cpp_symbols: Some(vec![CPP::unset("STAT_MACROS_BROKEN")]),
                    paths: Some(vec!["sys/stat.h".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_HEADER_STDBOOL",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_internal("ac_cv_header_stdbool_h").yes()]),
                    cpp_symbols: Some(vec![CPP::set("HAVE_STDBOOL_H"), CPP::set("HAVE__BOOL")]),
                    paths: Some(vec!["stdbool.h".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_HEADER_STDC",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    require: Some(vec![
                        "AC_CHECK_INCLUDES_DEFAULT".into(),
                        "AC_PROG_EGREP".into(),
                    ]),
                    ..Default::default()
                },
            ),
            (
                "AC_STDC_HEADERS", // obsolete. replaced by `AC_HEADER_STDC` except few changes.
                M4MacroSignature {
                    replaced_by: Some("AC_HEADER_STDC".into()),
                    ..Default::default()
                },
            ),
            (
                "AC_HEADER_SYS_WAIT",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_internal("ac_cv_header_sys_wait_h").yes()]),
                    cpp_symbols: Some(vec![
                        CPP::set("HAVE_SYS_WAIT_H"),
                        // if 'unistd.h' is included on Posix systems
                        CPP::with_value("_POSIX_VERSION", "200809L"),
                    ]),
                    paths: Some(vec!["sys/wait.h".into(), "unistd.h".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_HEADER_TIOCGWINSZ",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        Var::define_internal("ac_cv_sys_tiocgwinsz_in_termios_h").no(),
                        Var::define_internal("ac_cv_sys_tiocgwinsz_in_sys_ioctl_h").yes(),
                    ]),
                    cpp_symbols: Some(vec![CPP::set("GWINSZ_IN_SYS_IOCTL")]),
                    paths: Some(vec!["termios.h".into(), "sys/ioctl.h".into()]),
                    ..Default::default()
                },
            ),
            // Generic header checks
            (
                "AC_CHECK_HEADER",
                M4MacroSignature {
                    arg_types: vec![
                        Path(Some(&|s| {
                            // header-file
                            vec![ExVar(
                                VarAttrs::internal(),
                                format!("ac_cv_header_{}", sanitize_shell_name(s)),
                                None,
                            )]
                        })),
                        Cmds, // [action-if-found]
                        Cmds, // [action-if-not-found]
                        Prog, // [includes]
                    ],
                    shell_vars: Some(vec![
                        Var::reference("CPPFLAGS"),
                    ]),
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_CHECK_HEADERS",
                M4MacroSignature {
                    arg_types: vec![
                        Paths(
                            Blank,
                            Some(&|s| {
                                // header-file...
                                vec![
                                    ExVar(
                                        VarAttrs::internal(),
                                        format!("ac_cv_header_{}", sanitize_shell_name(s)),
                                        None,
                                    ),
                                    ExCPP(format!("HAVE_{}", sanitize_c_name(s)), None),
                                ]
                            }),
                        ),
                        Cmds, // [action-if-found]
                        Cmds, // [action-if-not-found]
                        Prog, // [includes]
                    ],
                    shell_vars: Some(vec![
                        Var::reference("CPPFLAGS"),
                    ]),
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_CHECK_HEADERS_ONCE",
                M4MacroSignature {
                    arg_types: vec![Paths(
                        Blank,
                        Some(&|s| {
                            // header-file...
                            vec![
                                ExVar(
                                    VarAttrs::internal(),
                                    format!("ac_cv_header_{}", sanitize_shell_name(s),),
                                    None,
                                ),
                                ExCPP(format!("HAVE_{}", sanitize_c_name(s)), None),
                            ]
                        }),
                    )],
                    ret_type: Some(Cmds),
                    require: Some(vec!["AC_CHECK_INCLUDES_DEFAULT".into()]),
                    ..Default::default()
                },
            ),
            // Generic declaration checks
            (
                "AC_CHECK_DECL",
                M4MacroSignature {
                    arg_types: vec![
                        Symbol(Some(&|s| {
                            // symbol
                            vec![ExVar(
                                VarAttrs::internal(),
                                format!("ac_cv_have_decl_{}", sanitize_shell_name(s)),
                                None,
                            )]
                        })),
                        Cmds, // [action-if-found]
                        Cmds, // [action-if-not-found]
                        Prog, // [includes=AC_INCLUDES_DEFAULT]
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_CHECK_DECLS",
                M4MacroSignature {
                    arg_types: vec![
                        Symbols(
                            Comma,
                            Some(&|s| {
                                // symbol
                                vec![
                                    ExVar(
                                        VarAttrs::internal(),
                                        format!("ac_cv_have_decl_{}", sanitize_shell_name(s)),
                                        None,
                                    ),
                                    ExCPP(format!("HAVE_DECL_{}", sanitize_shell_name(s)), None),
                                ]
                            }),
                        ),
                        Cmds, // [action-if-found]
                        Cmds, // [action-if-not-found]
                        Prog, // [includes=AC_INCLUDES_DEFAULT]
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_CHECK_DECLS_ONCE",
                M4MacroSignature {
                    arg_types: vec![Symbols(
                        Comma,
                        Some(&|s| {
                            // symbols
                            vec![
                                ExVar(
                                    VarAttrs::internal(),
                                    format!("ac_cv_have_decl_{}", sanitize_shell_name(s)),
                                    None,
                                ),
                                ExCPP(format!("HAVE_DECL_{}", sanitize_shell_name(s)), None),
                            ]
                        }),
                    )],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            // Paticular structure checks
            (
                "AC_STRUCT_DIRENT_D_INO",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    cpp_symbols: Some(vec![
                        // if `struct dirent.d_ino` exists
                        CPP::set("HAVE_STRUCT_DIRENT_D_INO"),
                    ]),
                    require: Some(vec!["AC_HEADER_DIRENT".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_STRUCT_DIRENT_D_TYPE",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    cpp_symbols: Some(vec![
                        // if `struct dirent.d_type` exists
                        CPP::set("HAVE_STRUCT_DIRENT_D_TYPE"),
                    ]),
                    require: Some(vec!["AC_HEADER_DIRENT".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_STRUCT_ST_BLOCKS",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_internal(
                        "ac_cv_member_struct_stat_st_blocks",
                    )
                    .yes()]),
                    cpp_symbols: Some(vec![
                        CPP::set("HAVE_STRUT_ST_BLOCKS"),
                        CPP::set("HAVE_ST_BLOCKS"), // deprecated
                    ]),
                    paths: Some(vec![
                        // if `struct stat.st_blocks` does not exists
                        "fileblocks.c".into(),
                    ]),
                    ..Default::default()
                },
            ),
            (
                "AC_STRUCT_TM",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    cpp_symbols: Some(vec![CPP::unset("TM_IN_SYS_TIME")]),
                    paths: Some(vec!["sys/time.h".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_STRUCT_TIMEZONE",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    cpp_symbols: Some(vec![
                        CPP::set("HAVE_STRUCT_TM_TM_ZONE"),
                        CPP::set("HAVE_TM_ZONE"), // deprecated
                        // `struct tm.tm_zone` is not found
                        // and if the external array 'tzname' is found
                        CPP::unset("HAVE_TZNAME"),      // if defined
                        CPP::unset("HAVE_DECL_TZNAME"), // if declared
                    ]),
                    paths: Some(vec!["sys/time.h".into()]),
                    ..Default::default()
                },
            ),
            // Generic structure checks
            (
                "AC_CHECK_MEMBER",
                M4MacroSignature {
                    arg_types: vec![
                        Type(Some(&|s| {
                            // abbregate.member
                            vec![ExVar(
                                VarAttrs::internal(),
                                format!("ac_cv_member_{}", sanitize_shell_name(s)),
                                None,
                            )]
                        })),
                        Cmds, // [action-if-found]
                        Cmds, // [action-if-not-found]
                        Prog, // [includes=AC_INCLUDES_DEFAULT]
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_CHECK_MEMBERS",
                M4MacroSignature {
                    arg_types: vec![
                        Types(
                            Comma,
                            Some(&|s| {
                                // abbregate.member
                                vec![
                                    ExVar(
                                        VarAttrs::internal(),
                                        format!("ac_cv_member_{}", sanitize_shell_name(s)),
                                        None,
                                    ),
                                    ExCPP(format!("HAVE_{}", sanitize_c_name(s)), None),
                                ]
                            }),
                        ),
                        Cmds, // [action-if-found]
                        Cmds, // [action-if-not-found]
                        Prog, // [includes=AC_INCLUDES_DEFAULT]
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            // Paticular type checks
            (
                "AC_TYPE_GETGROUPS",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        Var::define_internal("ac_cv_type_getgroups").with_value("gid_t")
                    ]),
                    cpp_symbols: Some(vec![CPP::with_value("GETGROUPS_T", "gid_t")]),
                    paths: Some(vec!["unistd.h".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_TYPE_INT8_T",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_internal("ac_cv_c_int8_t").yes()]),
                    cpp_symbols: Some(vec![CPP::unset("int8_t")]),
                    paths: Some(vec!["stdint.h".into(), "inttypes.h".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_TYPE_INT16_T",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_internal("ac_cv_c_int16_t").yes()]),
                    cpp_symbols: Some(vec![CPP::unset("int16_t")]),
                    paths: Some(vec!["stdint.h".into(), "inttypes.h".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_TYPE_INT32_T",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_internal("ac_cv_c_int32_t").yes()]),
                    cpp_symbols: Some(vec![CPP::unset("int32_t")]),
                    paths: Some(vec!["stdint.h".into(), "inttypes.h".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_TYPE_INT64_T",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_internal("ac_cv_c_int64_t").yes()]),
                    cpp_symbols: Some(vec![CPP::unset("int64_t")]),
                    paths: Some(vec!["stdint.h".into(), "inttypes.h".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_TYPE_INTMAX_T",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_internal("ac_cv_type_intmax_t").yes()]),
                    cpp_symbols: Some(vec![CPP::set("HAVE_INTMAX_T"), CPP::unset("intmax_t")]),
                    paths: Some(vec!["stdint.h".into(), "inttypes.h".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_TYPE_INTPTR_T",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_internal("ac_cv_type_intptr_t").yes()]),
                    cpp_symbols: Some(vec![CPP::set("HAVE_INTPTR_T"), CPP::unset("intptr_t")]),
                    paths: Some(vec!["stdint.h".into(), "inttypes.h".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_TYPE_LONG_DOUBLE",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_internal("ac_cv_type_long_double").yes()]),
                    cpp_symbols: Some(vec![CPP::set("HAVE_LONG_DOUBLE")]),
                    paths: Some(vec!["stdint.h".into(), "inttypes.h".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_TYPE_LONG_DOUBLE_WIDER",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        Var::define_internal("ac_cv_type_long_double_wider").yes()
                    ]),
                    cpp_symbols: Some(vec![CPP::set("HAVE_LONG_DOUBLE_WIDER")]),
                    paths: Some(vec!["stdint.h".into(), "inttypes.h".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_TYPE_LONG_LONG_INT",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_internal("ac_cv_type_long_long_int").yes()]),
                    cpp_symbols: Some(vec![CPP::set("HAVE_LONG_LONG_INT")]),
                    paths: Some(vec!["stdint.h".into(), "inttypes.h".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_TYPE_MBSTATE_T",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_internal("ac_cv_type_mbstate_t").yes()]),
                    cpp_symbols: Some(vec![CPP::set("HAVE_MBSTATE_T"), CPP::set("mbstate_t")]),
                    paths: Some(vec!["wchar.h".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_TYPE_MODE_T",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_internal("ac_cv_type_mode_t").yes()]),
                    cpp_symbols: Some(vec![CPP::unset("mode_t")]),
                    ..Default::default()
                },
            ),
            (
                "AC_TYPE_OFF_T",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_internal("ac_cv_type_off_t").yes()]),
                    cpp_symbols: Some(vec![CPP::unset("off_t")]),
                    ..Default::default()
                },
            ),
            (
                "AC_TYPE_PID_T",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_internal("ac_cv_type_pid_t").yes()]),
                    cpp_symbols: Some(vec![CPP::unset("pid_t")]),
                    ..Default::default()
                },
            ),
            (
                "AC_TYPE_SIZE_T",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_internal("ac_cv_type_size_t").yes()]),
                    cpp_symbols: Some(vec![CPP::unset("size_t")]),
                    ..Default::default()
                },
            ),
            (
                "AC_TYPE_SSIZE_T",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_internal("ac_cv_type_ssize_t").yes()]),
                    cpp_symbols: Some(vec![CPP::unset("ssize_t")]),
                    ..Default::default()
                },
            ),
            (
                "AC_TYPE_UID_T",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_internal("ac_cv_type_uid_t").yes()]),
                    cpp_symbols: Some(vec![CPP::unset("uid_t")]),
                    ..Default::default()
                },
            ),
            (
                "AC_TYPE_UINT8_T",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_internal("ac_cv_c_uint8_t").yes()]),
                    cpp_symbols: Some(vec![CPP::unset("uint8_t")]),
                    paths: Some(vec!["stdint.h".into(), "inttypes.h".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_TYPE_UINT16_T",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_internal("ac_cv_c_uint16_t").yes()]),
                    cpp_symbols: Some(vec![CPP::unset("uint16_t")]),
                    paths: Some(vec!["stdint.h".into(), "inttypes.h".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_TYPE_UINT32_T",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_internal("ac_cv_c_uint32_t").yes()]),
                    cpp_symbols: Some(vec![CPP::unset("uint32_t")]),
                    paths: Some(vec!["stdint.h".into(), "inttypes.h".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_TYPE_UINT64_T",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_internal("ac_cv_c_uint64_t").yes()]),
                    cpp_symbols: Some(vec![CPP::unset("uint64_t")]),
                    paths: Some(vec!["stdint.h".into(), "inttypes.h".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_TYPE_UINTMAX_T",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_internal("ac_cv_type_uintmax_t").yes()]),
                    cpp_symbols: Some(vec![CPP::set("HAVE_UINTMAX_T"), CPP::unset("uintmax_t")]),
                    paths: Some(vec!["stdint.h".into(), "inttypes.h".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_TYPE_UINTPTR_T",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_internal("ac_cv_type_uintptr_t").yes()]),
                    cpp_symbols: Some(vec![CPP::set("HAVE_UINTPTR_T"), CPP::unset("uintptr_t")]),
                    paths: Some(vec!["stdint.h".into(), "inttypes.h".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_TYPE_UNSIGNED_LONG_LONG_T",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_internal(
                        "ac_cv_type_unsigned_long_long_int",
                    )
                    .yes()]),
                    cpp_symbols: Some(vec![CPP::set("HAVE_UNSIGNED_LONG_LONG_INT")]),
                    paths: Some(vec!["stdint.h".into(), "inttypes.h".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_TYPE_SIGNAL",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: None,
                    cpp_symbols: Some(vec![CPP::with_value("RETSIGTYPE", "void")]),
                    paths: Some(vec!["signal.h".into()]),
                    ..Default::default()
                },
            ),
            // Generic type checks
            (
                "AC_CHECK_TYPE",
                // Autoconf up to 2.13 used a different signature for this macro:
                // AC_CHECCK_TYPE(TYPE, DEFAULT). But due to the limitation of the current
                // implementation, we just ignore it and always try to parse the second arg as Cmds.
                M4MacroSignature {
                    arg_types: vec![
                        Type(Some(&|s| {
                            // type
                            vec![ExVar(
                                VarAttrs::internal(),
                                format!(
                                    "ac_cv_type_{}",
                                    sanitize_shell_name(s.replace("*", "p").as_ref())
                                ),
                                None,
                            )]
                        })),
                        Cmds, // [action-if-found]
                        Cmds, // [action-if-not-found]
                        Prog, // [includes=AC_INCLUDES_DEFAULT]
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_CHECK_TYPES",
                M4MacroSignature {
                    arg_types: vec![
                        Types(
                            Comma,
                            Some(&|s| {
                                // types
                                vec![
                                    ExVar(
                                        VarAttrs::internal(),
                                        format!(
                                            "ac_cv_type_{}",
                                            sanitize_shell_name(s.replace("*", "p").as_ref())
                                        ),
                                        None,
                                    ),
                                    ExCPP(format!("HAVE_{}", sanitize_c_name(s)), None),
                                ]
                            }),
                        ),
                        Cmds, // [action-if-found]
                        Cmds, // [action-if-not-found]
                        Prog, // [includes=AC_INCLUDES_DEFAULT]
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ), // Generic compiler characteristics
            (
                "AC_CHECK_SIZEOF",
                M4MacroSignature {
                    arg_types: vec![
                        Type(Some(&|s| {
                            // type-or-expr
                            vec![
                                ExVar(
                                    VarAttrs::internal(),
                                    format!(
                                        "ac_cv_sizeof_{}",
                                        sanitize_shell_name(s.replace("*", "p").as_ref())
                                    ),
                                    None,
                                ),
                                ExCPP(format!("SIZE_OF_{}", sanitize_c_name(s)), None),
                            ]
                        })),
                        Lit,  // [unused]
                        Prog, // [includes=AC_INCLUDES_DEFAULT]
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_CHECK_ALIGNOF",
                M4MacroSignature {
                    arg_types: vec![
                        Type(Some(&|s| {
                            // type-or-expr
                            vec![
                                ExVar(
                                    VarAttrs::internal(),
                                    format!(
                                        "ac_cv_align_of_{}",
                                        sanitize_shell_name(s.replace("*", "p").as_ref())
                                    ),
                                    None,
                                ),
                                ExCPP(format!("ALIGN_OF_{}", sanitize_c_name(s)), None),
                            ]
                        })),
                        Prog, // [includes=AC_INCLUDES_DEFAULT]
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_COMPUTE_INT",
                M4MacroSignature {
                    arg_types: vec![
                        VarName(Some(VarAttrs::internal()), None), // var
                        Lit,  // expression (it is not `Prog` but a part of C program (r-value).)
                        Prog, // [includes=AC_INCLUDES_DEFAULT]
                        Cmds, // [action-if-fails]
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_LANG_WERROR",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_OPENMP",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // [language]
                    ],
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        Var::define_output("OPENMP_CFLAGS"),
                        Var::define_output("OPENMP_CXXFLAGS"),
                        Var::define_output("OPENMP_FFLAGS"),
                        Var::define_output("OPENMP_FCFLAGS"),
                        Var::define_output("OPENMP_FFLAGS"),
                    ]),
                    cpp_symbols: Some(vec!["_OPENMP".into()]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AC_PROG_CC",
                M4MacroSignature {
                    arg_types: vec![
                        Arr(Blank), // compiler-search-list
                    ],
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        Var::define_env("CC").with_value("cc"),
                        Var::define_precious("CFLAGS").with_value("-g -O2"), // -g -O2
                        Var::define_precious("LDFLAGS").with_value(""),
                        Var::define_precious("LIBS").with_value(""),
                        Var::define_precious("OBJC"),
                        Var::define_output("OBJEXT").with_value("o"),
                        Var::define_output("ac_prog_cc_stdc").with_value("c11"), // c11/c99/c89/no
                        Var::define_internal("GCC").yes(), // set to 'yes' if the selected compiler is GNU C
                        Var::reference("ac_tool_prefix"),
                    ]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AC_PROG_CC_C89", // obsolete. done by AC_PROG_CC
                M4MacroSignature {
                    replaced_by: Some("AC_PROG_CC".into()),
                    ..Default::default()
                },
            ),
            (
                "AC_PROG_CC_C99", // obsolete. done by AC_PROG_CC
                M4MacroSignature {
                    replaced_by: Some("AC_PROG_CC".into()),
                    ..Default::default()
                },
            ),
            (
                "AC_PROG_CC_STDC", // obsolete. done by AC_PROG_CC
                M4MacroSignature {
                    replaced_by: Some("AC_PROG_CC".into()),
                    ..Default::default()
                },
            ),
            (
                "AC_PROG_GCC_TRADITIONAL", // obsolete. done by AC_PROG_CC
                M4MacroSignature {
                    replaced_by: Some("AC_PROG_CC".into()),
                    ..Default::default()
                },
            ),
            (
                "AC_PROG_CC_C_O",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    // FIXME: shell variable `ac_cv_prog_cc_COMPILER_c_o` is actually defined,
                    // where COMPILER is the compiler name found by AC_PROG_CC.
                    cpp_symbols: Some(vec![CPP::unset("NO_MINUS_C_MINUS_O")]),
                    require: Some(vec!["AC_PROG_CC".into()]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AC_PROG_CPP",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        Var::define_env("CPP").with_value("cc -E"),
                        Var::define_precious("CPPFLAGS").with_value(""),
                        Var::reference("CC"),
                        Var::define_internal("ac_cv_prog_CPP").with_value("cc -E"),
                    ]),
                    cpp_symbols: Some(vec![CPP::unset("NO_MINUS_C_MINUS_O")]),
                    require: Some(vec!["AC_PROG_CC".into()]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AC_PROG_CPP_WERROR",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        // turning it on will make CPP treats warnings as errors
                        Var::define_internal("ac_c_preproc_warn_flag").yes(),
                    ]),
                    require: Some(vec!["AC_PROG_CPP".into()]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AC_C_BACKSLASH_A",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    cpp_symbols: Some(vec![CPP::set("HAVE_C_BACKSLASH_A")]),
                    ..Default::default()
                },
            ),
            (
                "AC_C_BIGENDIAN",
                M4MacroSignature {
                    arg_types: vec![
                        Cmds, // [action-if-true]
                        Cmds, // [action-if-false]
                        Cmds, // [action-if-unknown]
                        Cmds, // [action-if-universal]
                    ],
                    ret_type: Some(Cmds),
                    cpp_symbols: Some(vec![
                        "WITH_BIGENDIAN".into(),
                        "AC_APPLE_UNIVERSAL_BUILD".into(),
                    ]),
                    ..Default::default()
                },
            ),
            (
                "AC_C_CONST",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_internal("ac_cv_c_const").yes()]),
                    cpp_symbols: Some(vec![
                        // Define to empty if 'const' does not conform to ANSI C.
                        CPP::unset("const"),
                    ]),
                    ..Default::default()
                },
            ),
            (
                "AC_C__GENERIC",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_internal("ac_cv_c__Generic").yes()]),
                    cpp_symbols: Some(vec![
                        // Define to 1 if C11-style _Generic works.
                        CPP::set("HAVE_C__GENERIC"),
                    ]),
                    ..Default::default()
                },
            ),
            (
                "AC_C_RESTRICT",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    cpp_symbols: Some(vec![
                        // Define to the alternate spelling of 'restrict' keyword.
                        CPP::unset("restrict"),
                    ]),
                    ..Default::default()
                },
            ),
            (
                "AC_C_VOLATILE",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    cpp_symbols: Some(vec![
                        // Define to empty if keyword 'volatile' does not work.
                        CPP::unset("volatile"),
                    ]),
                    ..Default::default()
                },
            ),
            (
                "AC_C_INLINE",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_internal("ac_cv_c_inline").yes()]),
                    cpp_symbols: Some(vec![
                        // Define to '__inline__', '__inline', or empty.
                        CPP::unset("inline"),
                    ]),
                    ..Default::default()
                },
            ),
            (
                "AC_INLINE",
                M4MacroSignature {
                    replaced_by: Some("AC_C_INLINE".into()),
                    ..Default::default()
                },
            ),
            (
                "AC_C_CHAR_UNSIGNED",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    cpp_symbols: Some(vec![
                        // Define to 1 if type 'char' is unsigned.
                        "__CHAR_UNSIGNED__".into(),
                    ]),
                    ..Default::default()
                },
            ),
            (
                "AC_C_STRINGIZE",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    cpp_symbols: Some(vec![
                        // Define to 1 if the preprocessor supports the ANSI's '#' operator.
                        CPP::set("HAVE_STRINGSIZE"),
                    ]),
                    ..Default::default()
                },
            ),
            (
                "AC_C_FLEXIBLE_ARRAY_MEMBER",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    cpp_symbols: Some(vec![
                        // Define to nothing if C SUPPORTS flexible array members,
                        // and to 1 if it does NOT.
                        CPP::with_value("FLEXIBLE_ARRAY_MEMBER", ""),
                    ]),
                    ..Default::default()
                },
            ),
            (
                "AC_C_VARARRAYS",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    cpp_symbols: Some(vec![
                        CPP::set("HAVE_C_VARARRAYS"),  // if supported
                        CPP::unset("__STDC_NO_VLA__"), // if not supported
                    ]),
                    ..Default::default()
                },
            ),
            (
                "AC_C_TYPEOF",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    cpp_symbols: Some(vec![
                        // Define to 1 if typeof works with the compiler.
                        CPP::set("HAVE_TYPEOF"),
                        // Define to __typeof__ if the compiler spells it that way.
                        CPP::unset("typeof"),
                    ]),
                    ..Default::default()
                },
            ),
            (
                "AC_C_PROTOTYPES", // obsolete. we could assume C89 compliance.
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    cpp_symbols: Some(vec![
                        // Define to 1 if the C compiler supports function prototypes.
                        CPP::set("PROTOTYPES"),
                        // Define like PROTOTYPES; this can be used by system headers.
                        CPP::set("__PROTOTYPES"),
                    ]),
                    ..Default::default()
                },
            ), // TODO: do below
            // C++ compiler characteristics
            (
                "AC_PROG_CXX",
                M4MacroSignature {
                    arg_types: vec![
                        Arr(Blank), // [compiler-search-list]
                    ],
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        Var::define_env("CXX").with_value("c++"),
                        Var::define_env("CCC").with_value("c++"),
                        Var::define_precious("CXXFLAGS").with_value("-g -O2"),
                        Var::define_precious("LDFLAGS").with_value(""),
                        Var::define_precious("LIBS").with_value(""),
                        Var::define_internal("GXX").yes(), // set to yes if gnu compiler is found
                        // cxx11/cxx98/no
                        Var::define_output("ac_prog_cxx_stdcxx"),
                    ]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AC_PROG_CXXCPP",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        Var::define_precious("CXXPP").with_value("c++ -E"),
                        Var::define_precious("CXXFLAGS").with_value(""),
                        Var::reference("CXX"),
                        "ac_cv_prog_CXXPP".into(),
                    ]),
                    require: Some(vec!["AC_PROG_CXX".into()]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AC_PROG_CXX_C_O",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    cpp_symbols: Some(vec![CPP::unset("CXX_NO_MINUS_C_MINUS_O")]),
                    require: Some(vec!["AC_PROG_CXX".into()]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            // TODO: refine the macros related to languages
            // other then C/CXX (that I am not interested in now)

            // Objective C compiler characteristics
            (
                "AC_PROG_OBJC",
                M4MacroSignature {
                    arg_types: vec![Arr(Blank)],
                    ret_type: Some(Cmds),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AC_PROG_OBJCPP",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            // Objective C compiler characteristics
            (
                "AC_PROG_OBJCXX",
                M4MacroSignature {
                    arg_types: vec![Arr(Blank)],
                    ret_type: Some(Cmds),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AC_PROG_OBJCXXCPP",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            // Erlang compiler and interpreter characteristics
            (
                "AC_ERLANG_PATH_ERLC",
                M4MacroSignature {
                    arg_types: vec![Lit, Lit],
                    ret_type: Some(Cmds),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AC_ERLANG_NEED_ERLC",
                M4MacroSignature {
                    arg_types: vec![Lit],
                    ret_type: Some(Cmds),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AC_ERLANG_PATH_ERL",
                M4MacroSignature {
                    arg_types: vec![Lit, Lit],
                    ret_type: Some(Cmds),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AC_ERLANG_NEED_ERL",
                M4MacroSignature {
                    arg_types: vec![Lit],
                    ret_type: Some(Cmds),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            // Fortran compiler characteristics
            (
                "AC_PROG_F77",
                M4MacroSignature {
                    arg_types: vec![Arr(Blank)],
                    ret_type: Some(Cmds),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AC_PROG_FC",
                M4MacroSignature {
                    arg_types: vec![Arr(Blank), Lit],
                    ret_type: Some(Cmds),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AC_PROG_F77_C_O",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AC_PROG_FC_C_O",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_F77_LIBRARY_LDFLAGS",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_FC_LIBRARY_LDFLAGS",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_F77_DUMMY_MAIN",
                M4MacroSignature {
                    arg_types: vec![Cmds, Cmds, Cmds],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_FC_DUMMY_MAIN",
                M4MacroSignature {
                    arg_types: vec![Cmds, Cmds, Cmds],
                    ret_type: Some(Cmds),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AC_F77_MAIN",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_FC_MAIN",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_F77_WRAPPERS",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_FC_WRAPPERS",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_F77_FUNC",
                M4MacroSignature {
                    arg_types: vec![Lit, Lit],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_FC_FUNC",
                M4MacroSignature {
                    arg_types: vec![Lit, Lit],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_FC_SRCEXT",
                M4MacroSignature {
                    arg_types: vec![Lit, Cmds, Cmds],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_FC_PP_SRCEXT",
                M4MacroSignature {
                    arg_types: vec![Lit, Cmds, Cmds],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_FC_PP_DEFINE",
                M4MacroSignature {
                    arg_types: vec![Cmds, Cmds],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_FC_FIXEDFORM",
                M4MacroSignature {
                    arg_types: vec![Cmds, Cmds],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_FC_LINE_LENGTH",
                M4MacroSignature {
                    arg_types: vec![Lit, Cmds, Cmds],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_FC_CHECK_LENGTH",
                M4MacroSignature {
                    arg_types: vec![Cmds, Cmds],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_F77_IMPLICIT_NONE",
                M4MacroSignature {
                    arg_types: vec![Cmds, Cmds],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_FC_IMPLICIT_NONE",
                M4MacroSignature {
                    arg_types: vec![Cmds, Cmds],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_FC_MODULE_EXTENSION",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_FC_MODULE_FLAG",
                M4MacroSignature {
                    arg_types: vec![Cmds, Cmds],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_FC_MODULE_OUTPUT_FLAG",
                M4MacroSignature {
                    arg_types: vec![Cmds, Cmds],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_F77_CRAY_POINTERS",
                M4MacroSignature {
                    arg_types: vec![Cmds, Cmds],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_FC_CRAY_POINTERS",
                M4MacroSignature {
                    arg_types: vec![Cmds, Cmds],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            // Go compiler characteristics
            (
                "AC_PROG_GO",
                M4MacroSignature {
                    arg_types: vec![Arr(Blank)],
                    shell_vars: Some(vec![
                        Var::define_precious("GOC"),
                        Var::define_precious("GOFLAGS"),
                    ]),
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            // System services
            (
                "AC_PATH_X",
                // X denotes the X Window System
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        Var::define_input("have_x"),
                        "no_x".into(),
                        "x_includes".into(),
                        "x_libraries".into(),
                        Var::define_precious("XMKMF"),
                    ]),
                    require: Some(vec!["AC_PROG_CC".into()]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AC_PATH_XTRA",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        Var::define_output("X_CFLAGS"),
                        Var::define_output("X_PRE_LIBS"),
                        Var::define_output("X_LIBS"),
                        Var::define_output("X_EXTRA_LIBS"),
                        // by AC_CHECK_...
                        "ac_cv_func_gethostbyname".into(),
                        "ac_cv_lib_nsl_gethostbyname".into(),
                        "ac_cv_lib_bsd_gethostbyname".into(),
                        "ac_cv_func_connect".into(),
                        "ac_cv_lib_socket_connect".into(),
                        "ac_cv_func_remove".into(),
                        "ac_cv_lib_posix_remove".into(),
                        "ac_cv_func_shmat".into(),
                        "ac_cv_lib_ipc_shmat".into(),
                        "ac_cv_lib_ICE_IceConnectionNumber".into(),
                    ]),
                    cpp_symbols: Some(vec!["X_DISPLAY_MISSING".into()]),
                    require: Some(vec!["AC_PATH_X".into()]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AC_SYS_INTERPRETER",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        // Define to 'yes' if system supports `#!` in the script.
                        Var::define_internal("interpval").yes(),
                    ]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AC_SYS_LARGEFILE",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        Var::define_input("enable_largefile").yes(),
                        Var::define_input("enable_year2038").yes(),
                        // set to 'yes' if a wide `off_t` is available.
                        Var::define_internal("ac_have_largefile").yes(),
                        // set to 'yes' if a wide `time_t` is available.
                        Var::define_internal("ac_have_year2038").yes(),
                    ]),
                    cpp_symbols: Some(vec![
                        // Define to 64 on hosts where this is settable
                        CPP::with_value("_FILE_OFFSET_BITS", "64"),
                        // Define to 1 on platforms where this makes off_t a 64-bit type
                        CPP::set("_LARGE_FILES"),
                        // by _AC_SYS_YEAR2038_PROBE
                        // Define to 64 on on hosts where this is settable.
                        CPP::with_value("_TIME_BITS", "64"),
                        // Define to 1 on platforms where this makes time_t a 64-bit type.
                        CPP::set("__MINGW_USE_VC2005_COMPAT"),
                    ]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AC_SYS_LONG_FILE_NAMES",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    cpp_symbols: Some(vec![
                        // Define if the system supports file names longer than 14 chars.
                        CPP::set("HAVE_LONG_FILE_NAMES"),
                    ]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AC_SYS_POSIX_TERMIOS",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        // Set to 'yes' if 'termios.h' and 'tcgetattr' are supported.
                        Var::define_internal("ac_cv_sys_posix_termios").yes(),
                    ]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AC_SYS_YEAR2038",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    require: Some(vec!["AC_SYS_LARGEFILE".into()]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AC_SYS_YEAR2038_RECOMMENDED",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    require: Some(vec!["AC_SYS_LARGEFILE".into()]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            // C and posix variants
            (
                "AC_USE_SYSTEM_EXTENSIONS",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    cpp_symbols: Some(vec![
                        // Defined unconditionally
                        CPP::set("_ALL_SOURCE"),
                        CPP::set("_DARWIN_C_SOURCCE"),
                        CPP::set("_GNU_SOURCER"),
                        CPP::set("_NETBSD_SOURCE"),
                        CPP::set("_OPENBSD_SOURCE"),
                        CPP::set("_POSIX_PTHREAD_SEMANTICS"),
                        CPP::set("__STDC_WANT_IEC_60559_ATTRIBS_EXT__"),
                        CPP::set("__STDC_WANT_IEC_60559_BEP_EXT__"),
                        CPP::set("__STDC_WANT_IEC_60559_DEP_EXT__"),
                        CPP::set("__STDC_WANT_IEC_60559_EXT__"),
                        CPP::set("__STDC_WANT_IEC_60559_FUNCS_EXT__"),
                        CPP::set("__STDC_WANT_IEC_60559_TYPES_EXT__"),
                        CPP::set("__STDC_WANT_LIB_EXT2__"),
                        CPP::set("__STDC_WANT_MATH_SPEC_FUNCS__"),
                        CPP::set("__TANDEM_SOURCE"),
                        // Defined occasionally
                        CPP::unset("__EXTENSIONS__"),
                        CPP::unset("__MINIX"),
                        CPP::unset("__POSIX_SOURCE"),
                        CPP::unset("__POISIX_1_SOURCE"),
                        // Define to 500 only if needed to make 'wchar.h' declare 'mbstate_t'.
                        // This is known to be needed on some versions of HP/UX.
                        CPP::unset("__XOPEN_SOURCE"),
                        // by _AC_CHECK_HEADER_ONCE
                        CPP::set("HAVE_WCHAR_H"),
                        CPP::unset("HAVE_MINIX_CONFIG_H"),
                    ]),
                    paths: Some(vec!["wchar.h".into(), "minix/config.h".into()]),
                    require: Some(vec!["AC_CHECK_INCLUDES_DEFAULT".into()]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AC_GNU_SOURCE",
                M4MacroSignature {
                    replaced_by: Some("AC_USE_SYSTEM_EXTENSIONS".into()),
                    ..Default::default()
                },
            ),
            // @kui8shi
            // Skip Erlang thigs for now because the priority is relatively low.
            // Erlang libraries
            (
                "AC_ERLANG_SUBST_ERTS_VER",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_ERLANG_SUBST_ROOT_DIR",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_ERLANG_SUBST_LIB_DIR",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_ERLANG_CHECK_LIB",
                M4MacroSignature {
                    arg_types: vec![Lit, Cmds, Cmds],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_ERLANG_SUBST_INSTALL_LIB_DIR",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR",
                M4MacroSignature {
                    arg_types: vec![Lit, Lit],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            // Language choice
            (
                "AC_LANG",
                M4MacroSignature {
                    // Switch programming language to be used
                    // in the subsequent tests in configure.ac.
                    arg_types: vec![
                        // Supported languages are:
                        // 'C'
                        // 'C++'
                        // 'Fortran 77'
                        // 'Fortran'
                        // 'Erlang'
                        // 'Objective C'
                        // 'Objective C++'
                        // 'Go'
                        Word,
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "_AC_LANG_PREFIX",
                M4MacroSignature {
                    ret_type: Some(Lit),
                    ..Default::default()
                },
            ),
            (
                "AC_LANG_PUSH",
                M4MacroSignature {
                    arg_types: vec![Word],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_LANG_POP",
                M4MacroSignature {
                    arg_types: vec![Word],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_LANG_ASSERT",
                M4MacroSignature {
                    arg_types: vec![Word],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_REQUIRE_CPP",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    // @kui8shi
                    // what does this macro effectively do?
                    // I did not understand the detail of side effects even after reading.
                    // As far as I use this crate, I won't suffer btw.
                    ..Default::default()
                },
            ),
            // Generating sources
            (
                "AC_LANG_CONFTEST",
                M4MacroSignature {
                    arg_types: vec![Prog],
                    ret_type: Some(Cmds),
                    paths: Some(vec![
                        // actually the file extension depends on the context.
                        // we assume LANG is set to 'C' for now.
                        "conftest.c".into(),
                    ]),
                    ..Default::default()
                },
            ),
            (
                "AC_LANG_DEFINES_PROVIDED",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    // @kui8shi
                    // Idk what does it do.
                    // Especially the logics after _AC_LANG_DISPATCH is too complex for me.
                    ..Default::default()
                },
            ),
            (
                "AC_LANG_SOURCE",
                M4MacroSignature {
                    arg_types: vec![
                        Prog, // body
                    ],
                    ret_type: Some(Prog),
                    paths: Some(vec!["conftest.h".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_LANG_PROGRAM",
                M4MacroSignature {
                    arg_types: vec![
                        Prog, // prologue
                        Prog, // body
                    ],
                    ret_type: Some(Prog),
                    ..Default::default()
                },
            ),
            (
                "AC_LANG_CALL",
                M4MacroSignature {
                    arg_types: vec![
                        Prog, // prologue
                        Word, // function
                    ],
                    ret_type: Some(Prog),
                    ..Default::default()
                },
            ),
            (
                "AC_LANG_FUNC_LINK_TRY",
                M4MacroSignature {
                    arg_types: vec![
                        Word, // function
                    ],
                    ret_type: Some(Prog),
                    ..Default::default()
                },
            ),
            // Running the preprocessor
            (
                "AC_PREPROC_IFELSE",
                M4MacroSignature {
                    arg_types: vec![
                        Prog, // input
                        Cmds, // [action-if-true]
                        Cmds, // [action-if-false]
                    ],
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        // the macro uses the flags
                        Var::reference("CPPFLAGS"),
                    ]),
                    ..Default::default()
                },
            ),
            (
                "AC_EGREP_HEADER",
                M4MacroSignature {
                    arg_types: vec![
                        Lit,        // pattern
                        Path(None), // header-file
                        Cmds,       // [action-if-found]
                        Cmds,       // [action-if-not-found]
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_EGREP_CPP",
                M4MacroSignature {
                    arg_types: vec![
                        Lit,  // pattern
                        Prog, // program
                        Cmds, // [action-if-found]
                        Cmds, // [action-if-not-found]
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            // Running the compiler
            (
                "AC_COMPILE_IFELSE",
                M4MacroSignature {
                    arg_types: vec![
                        Prog, // input
                        Cmds, // [action-if-true]
                        Cmds, // [action-if-false]
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_TRY_COMPILE",
                M4MacroSignature {
                    arg_types: vec![
                        Prog, // includes
                        Prog, // function-body
                        Cmds, // [action-if-true]
                        Cmds, // [action-if-false]
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            // Running the linker
            (
                "AC_LINK_IFELSE",
                M4MacroSignature {
                    arg_types: vec![
                        Prog, // input
                        Cmds, // [action-if-true]
                        Cmds, // [action-if-false]
                    ],
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        // the macro uses the flags
                        Var::reference("LDFLAGS"),
                        Var::reference("LIBS"),
                    ]),
                    ..Default::default()
                },
            ),
            (
                "AC_TRY_LINK",
                M4MacroSignature {
                    arg_types: vec![
                        Prog, // includes
                        Prog, // function-body
                        Cmds, // [action-if-true]
                        Cmds, // [action-if-false]
                    ],
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        // the macro uses the flags
                        Var::reference("LDFLAGS"),
                        Var::reference("LIBS"),
                    ]),
                    ..Default::default()
                },
            ),
            // Checking runtime behavior
            (
                "AC_RUN_IFELSE",
                M4MacroSignature {
                    arg_types: vec![
                        Prog, // input
                        Cmds, // [action-if-true]
                        Cmds, // [action-if-false]
                        Cmds, // [action-if-cross-compiling=AC_MSG_FAILURE]
                    ],
                    shell_vars: Some(vec![
                        // the macro uses the flags
                        Var::reference("LDFLAGS"),
                        Var::reference("LIBS"),
                        // assert it is not cross-compiling
                        Var::reference("cross_compiling"),
                    ]),
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_TRY_RUN",
                M4MacroSignature {
                    // actually there is a difference that
                    // the first input is wrapped by AC_LANG_SOURCE.
                    replaced_by: Some("AC_RUN_IFELSE".into()),
                    ..Default::default()
                },
            ),
            // Defininig C preprocessor symbols
            (
                "AC_DEFINE",
                M4MacroSignature {
                    arg_types: vec![
                        CPPSymbol, // Note that this argument could not contain any shell vars.
                        Word,      // If it contains whiespace, things would be a tragedy.
                        Lit,       // description
                    ],
                    ret_type: Some(CPPSymbol),
                    ..Default::default()
                },
            ),
            (
                "AC_DEFINE_UNQUOTED",
                M4MacroSignature {
                    arg_types: vec![
                        CPPSymbol, // if not contains shell vars, this is just a Symbol.
                        Word,      // this also can be a shell var
                        Lit,       // description
                    ],
                    ret_type: Some(CPPSymbol),
                    ..Default::default()
                },
            ),
            // Setting output variables
            (
                "AC_SUBST",
                M4MacroSignature {
                    arg_types: vec![VarName(Some(VarAttrs::new(Output, Referenced)), None), Word],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AM_SUBST_NOTMAKE",
                M4MacroSignature {
                    arg_types: vec![VarName(Some(VarAttrs::new(Output, Referenced)), None), Word],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_SUBST_FILE",
                M4MacroSignature {
                    arg_types: vec![
                        // the contents of the file named by this argument
                        // will be substituted to @filename@
                        Path(Some(&|s| {
                            // filename
                            vec![ExVar(VarAttrs::new(Output, Referenced), s.into(), None)]
                        })),
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_ARG_VAR",
                M4MacroSignature {
                    arg_types: vec![
                        // variable
                        VarName(Some(VarAttrs::precious()), None),
                        Lit,
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            // Caching results
            (
                "AC_CACHE_VAL",
                M4MacroSignature {
                    arg_types: vec![VarName(Some(VarAttrs::internal()), None), Cmds],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_CACHE_CHECK",
                M4MacroSignature {
                    arg_types: vec![Lit, VarName(Some(VarAttrs::internal()), None), Cmds],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_CACHE_LOAD",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_CACHE_SAVE",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            // Printing messages
            (
                "AC_MSG_CHECKING",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // feature-description
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_MSG_RESULT",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // result-description
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_MSG_NOTICE",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // message
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_MSG_ERROR",
                M4MacroSignature {
                    arg_types: vec![
                        Lit,  // error-description
                        Word, // [exit-status=$?/1]
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_MSG_FAILURE",
                M4MacroSignature {
                    arg_types: vec![
                        Lit,  // error-description
                        Word, // [exit-status=$?/1]
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_MSG_WARN",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // probelm-description
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            // Redefined M4 macros
            (
                "m4_builtin",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // name
                        Lit, // [args...]
                    ],
                    repeat: Some((1, 1)),
                    ret_type: Some(Def),
                    ..Default::default()
                },
            ),
            (
                "m4_changecom",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // start
                        Lit, // end='NL'
                    ],
                    ret_type: Some(Ctrl),
                    ..Default::default()
                },
            ),
            (
                "m4_changequote",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // start=`
                        Lit, // end='
                    ],
                    ret_type: Some(Ctrl),
                    ..Default::default()
                },
            ),
            (
                "changequote",
                M4MacroSignature {
                    replaced_by: Some("m4_changequote".into()),
                    ..Default::default()
                },
            ),
            (
                "m4_debugfile",
                M4MacroSignature {
                    arg_types: vec![
                        // output all debug logs to the file
                        Path(None), // file
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "m4_debugmode",
                M4MacroSignature {
                    arg_types: vec![
                        // change the debug logging level
                        Lit, // flags
                    ],
                    ret_type: Some(Ctrl),
                    ..Default::default()
                },
            ),
            (
                "m4_decr",
                M4MacroSignature {
                    // decrement the number
                    arg_types: vec![
                        Lit, // number
                    ],
                    ret_type: Some(Lit),
                    ..Default::default()
                },
            ),
            (
                "m4_define",
                M4MacroSignature {
                    arg_types: vec![
                        Lit,  // name
                        Body, // body
                    ],
                    num_args_required: 2,
                    ret_type: Some(Def),
                    ..Default::default()
                },
            ), // The command parsing is delayed till expandsion
            (
                "m4_defun",
                M4MacroSignature {
                    replaced_by: Some("AC_DEFUN".into()),
                    ..Default::default()
                },
            ), // The command parsing is delayed till expandsion
            (
                "define",
                M4MacroSignature {
                    replaced_by: Some("m4_define".into()),
                    ..Default::default()
                },
            ), // The command parsing is delayed till expandsion
            (
                "m4_divnum",
                M4MacroSignature {
                    // expands to the current diversion
                    ret_type: Some(Lit),
                    ..Default::default()
                },
            ),
            (
                "m4_errprint",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // message, ...
                    ],
                    repeat: Some((0, 0)),
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "m4_esyscmd",
                M4MacroSignature {
                    // expands to the stdout of the shell command
                    arg_types: vec![
                        Cmds, // shell-command
                    ],
                    ret_type: Some(Lit),
                    ..Default::default()
                },
            ),
            (
                "esyscmd",
                M4MacroSignature {
                    replaced_by: Some("m4_esyscmd".into()),
                    ..Default::default()
                },
            ),
            (
                "m4_eval",
                M4MacroSignature {
                    // expands to the value of the expression.
                    // Although the expression itself has an unique syntax,
                    // we won't parse the detail.
                    arg_types: vec![
                        Lit, // expression
                        Lit, // [radix='10']
                        Lit, // [width]
                    ],
                    num_args_required: 1,
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "m4_format",
                M4MacroSignature {
                    // it's pretty like a printf of C.
                    arg_types: vec![
                        Lit, // format-string
                        Lit, // ...
                    ],
                    repeat: Some((1, 1)),
                    ret_type: Some(Lit),
                    ..Default::default()
                },
            ),
            (
                "m4_ifdef",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // name
                        Lit, // string-1
                        Lit, // string-2
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "ifdef",
                M4MacroSignature {
                    replaced_by: Some("m4_ifdef".into()),
                    ..Default::default()
                },
            ),
            (
                "m4_incr",
                M4MacroSignature {
                    // increment the number
                    arg_types: vec![
                        Lit, // number
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "m4_index",
                M4MacroSignature {
                    // expands to the index of the first occurrence of substring
                    arg_types: vec![
                        Lit, // string
                        Lit, // substring
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "m4_indir",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // name
                        Lit, // [args...]
                    ],
                    ret_type: None,
                    // We ignore this macro call.
                    ..Default::default()
                },
            ),
            (
                "m4_len",
                M4MacroSignature {
                    // return the length of a string.
                    // 0 if empty string on BSD.
                    arg_types: vec![Word],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "len",
                M4MacroSignature {
                    replaced_by: Some("m4_len".into()),
                    ..Default::default()
                },
            ),
            (
                "m4_pushdef",
                M4MacroSignature {
                    arg_types: vec![
                        Lit,  // name
                        Body, // [expansion]
                    ],
                    ret_type: Some(Def),
                    ..Default::default()
                },
            ),
            (
                "m4_shift",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // arg1, ...
                    ],
                    repeat: Some((0, 0)),
                    ret_type: Some(Args),
                    ..Default::default()
                },
            ),
            (
                "m4_substr",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // string
                        Lit, // from
                        Lit, // [length]
                    ],
                    ret_type: Some(Lit),
                    ..Default::default()
                },
            ),
            (
                "m4_syscmd",
                M4MacroSignature {
                    // unlike m4_esyscmd, it expands to void.
                    arg_types: vec![
                        Cmds, // shell-command
                    ],
                    ret_type: None,
                    ..Default::default()
                },
            ),
            (
                "m4_sysval",
                M4MacroSignature {
                    // expands to the exit status of the last shell command with syscmd or esyscmd.
                    ret_type: Some(Lit),
                    ..Default::default()
                },
            ),
            (
                "m4_traceoff",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // [names...]
                    ],
                    repeat: Some((0, 0)),
                    ret_type: Some(Ctrl),
                    ..Default::default()
                },
            ),
            (
                "m4_traceon",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // [names...]
                    ],
                    ret_type: Some(Ctrl),
                    ..Default::default()
                },
            ),
            (
                "m4_translit",
                M4MacroSignature {
                    // character translation
                    // if the length of chars exceeds that of replacement,
                    // the excess chars are removed.
                    arg_types: vec![
                        Lit, // string
                        Lit, // chars
                        Lit, // replacement
                    ],
                    ret_type: Some(Lit),
                    ..Default::default()
                },
            ),
            (
                "__file__",
                M4MacroSignature {
                    ret_type: Some(Lit),
                    ..Default::default()
                },
            ),
            (
                "__line__",
                M4MacroSignature {
                    ret_type: Some(Lit),
                    ..Default::default()
                },
            ),
            (
                "__oline__",
                M4MacroSignature {
                    ret_type: Some(Lit),
                    ..Default::default()
                },
            ),
            (
                "patsubst",
                M4MacroSignature {
                    replaced_by: Some("m4_bpatsubst".into()),
                    ..Default::default()
                },
            ),
            (
                "regexp",
                M4MacroSignature {
                    replaced_by: Some("m4_bregexp".into()),
                    ..Default::default()
                },
            ),
            (
                "m4_bpatsubst",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // string
                        Lit, // regexp
                        Lit, // [replacement]
                    ],
                    ret_type: Some(Lit),
                    ..Default::default()
                },
            ), // originally it is `patsubst` in m4
            (
                "m4_bregexp",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // string
                        Lit, // regexp
                        Lit, // [replacement]
                    ],
                    ret_type: Some(Lit),
                    ..Default::default()
                },
            ), // originally it is `regexp` in m4
            (
                "m4_copy",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // source
                        Lit, // dest
                    ],
                    ret_type: Some(Def),
                    ..Default::default()
                },
            ),
            (
                "m4_copy_force",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // source
                        Lit, // dest
                    ],
                    ret_type: Some(Def),
                    ..Default::default()
                },
            ),
            (
                "m4_rename",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // source
                        Lit, // dest
                    ],
                    ret_type: Some(Def),
                    ..Default::default()
                },
            ),
            (
                "m4_rename_force",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // source
                        Lit, // dest
                    ],
                    ret_type: Some(Def),
                    ..Default::default()
                },
            ),
            (
                "m4_defn",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // macro...
                    ],
                    repeat: Some((0, 0)),
                    ret_type: Some(Body),
                    ..Default::default()
                },
            ),
            (
                "m4_divert",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // diversion
                    ],
                    ret_type: Some(Ctrl),
                    ..Default::default()
                },
            ),
            (
                "m4_dumpdef",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // name...
                    ],
                    repeat: Some((0, 0)),
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "m4_dumpdefs",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // name...
                    ],
                    repeat: Some((0, 0)),
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "m4_esyscmd_s",
                M4MacroSignature {
                    arg_types: vec![
                        Cmds, // shell-command
                    ],
                    ret_type: Some(Lit),
                    ..Default::default()
                },
            ),
            (
                "m4_exit",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // exit-status
                    ],
                    ret_type: Some(Ctrl),
                    ..Default::default()
                },
            ),
            (
                "m4_if",
                M4MacroSignature {
                    arg_types: vec![
                        Word, // string-1
                        Word, // string-2
                        Cmds, // equal
                        Cmds, // [not-equal]
                    ],
                    ret_type: Some(Cmds),
                    repeat: Some((0, 2)),
                    ..Default::default()
                },
            ),
            (
                "ifelse",
                M4MacroSignature {
                    replaced_by: Some("m4_if".into()),
                    ..Default::default()
                },
            ),
            //varargs
            (
                "m4_include",
                M4MacroSignature {
                    arg_types: vec![
                        Path(None), // file
                    ],
                    ret_type: Some(Ctrl),
                    ..Default::default()
                },
            ),
            (
                "m4_sinclude",
                M4MacroSignature {
                    arg_types: vec![
                        Path(None), // file
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "m4_mkstemp",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // template
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "m4_maketemp",
                M4MacroSignature {
                    replaced_by: Some("m4_mkstemp".into()),
                    ..Default::default()
                },
            ),
            (
                "m4_popdef",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // macro...
                    ],
                    repeat: Some((0, 0)),
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "m4_undefine",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // macro...
                    ],
                    repeat: Some((0, 0)),
                    ret_type: Some(Def),
                    ..Default::default()
                },
            ),
            (
                "m4_undivert",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // diversion...
                    ],
                    repeat: Some((0, 0)),
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "m4_wrap",
                M4MacroSignature {
                    // output the texts(or cmds) at the last stage (when read EOF)
                    // multiple m4_wrap are ordered in FIFO (unlike m4_wrap_lifo)
                    arg_types: vec![
                        Lit, // text
                    ],
                    ret_type: Some(Ctrl),
                    ..Default::default()
                },
            ),
            (
                "m4_wrap_lifo",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // text
                    ],
                    ret_type: Some(Ctrl),
                    ..Default::default()
                },
            ),
            (
                "m4wrap",
                M4MacroSignature {
                    replaced_by: Some("m4_wrap".into()),
                    ..Default::default()
                },
            ),
            (
                "m4_assert",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // expression
                        Lit, // [exit-status=1]
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "m4_errprintn",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // message
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "m4_fatal",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // message
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "m4_location",
                M4MacroSignature {
                    ret_type: Some(Lit),
                    ..Default::default()
                },
            ),
            (
                "m4_warn",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // category
                        Lit, // message
                    ],
                    ret_type: Some(Lit),
                    ..Default::default()
                },
            ),
            (
                "m4_cleandivert",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // diversion...
                    ],
                    repeat: Some((0, 0)),
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "m4_divert_once",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // diversion
                        Lit, // [content]
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "m4_divert_pop",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // [diversion]
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "m4_divert_push",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // [diversion]
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "m4_divert_text",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // diversion
                        Lit, // [content]
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "m4_init",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "m4_bmatch",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // string
                        Lit, // regex-1
                        Lit, // value-1
                        Lit, // [default]
                    ],
                    ret_type: Some(Lit),
                    repeat: Some((1, 2)),
                    ..Default::default()
                },
            ), // varargs
            (
                "m4_bpatsubsts",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // string
                        Lit, // regex-1
                        Lit, // subst-1
                    ],
                    ret_type: Some(Lit),
                    repeat: Some((1, 2)),
                    ..Default::default()
                },
            ), // varargs
            (
                "m4_case",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // string
                        Lit, // value-1
                        Lit, // if-value-1
                        Lit, // [default]
                    ],
                    ret_type: Some(Lit), // TODO: confine this to Cmds
                    repeat: Some((1, 2)),
                    ..Default::default()
                },
            ), // varargs
            (
                "m4_cond",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // test-1
                        Lit, // value-1
                        Lit, // if-value-1
                        Lit, // [default]
                    ],
                    ret_type: Some(Lit),
                    repeat: Some((0, 2)),
                    ..Default::default()
                },
            ), // varargs
            (
                "m4_default",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // expr-1
                        Lit, // expr-2
                    ],
                    ret_type: Some(Lit),
                    ..Default::default()
                },
            ),
            (
                "m4_default_quoted",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // expr-1
                        Lit, // expr-2
                    ],
                    ret_type: Some(Lit),
                    ..Default::default()
                },
            ),
            (
                "m4_default_nblank",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // expr-1
                        Lit, // expr-2
                    ],
                    ret_type: Some(Lit),
                    ..Default::default()
                },
            ),
            (
                "m4_default_nblank_quoted",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // expr-1
                        Lit, // expr-2
                    ],
                    ret_type: Some(Lit),
                    ..Default::default()
                },
            ),
            (
                "m4_define_default",
                M4MacroSignature {
                    arg_types: vec![
                        Lit,  // expr-1
                        Body, // expr-2
                    ],
                    ret_type: Some(Def),
                    ..Default::default()
                },
            ),
            (
                "m4_ifblank",
                M4MacroSignature {
                    arg_types: vec![
                        Lit,  // cond
                        Cmds, // [if-blank]
                        Cmds, // [if-text]
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "m4_ifnblank",
                M4MacroSignature {
                    arg_types: vec![
                        Lit,  // cond
                        Cmds, // [if-text]
                        Cmds, // [if-blank]
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "m4_ifndef",
                M4MacroSignature {
                    arg_types: vec![
                        Lit,  // macro
                        Cmds, // [if-defined]
                        Cmds, // [if-not-defined]
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "m4_ifset",
                M4MacroSignature {
                    arg_types: vec![
                        Lit,  // macro
                        Cmds, // [if-true]
                        Cmds, // [if-false]
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "m4_ifval",
                M4MacroSignature {
                    arg_types: vec![
                        Lit,  // cond
                        Cmds, // [if-true]
                        Cmds, // [if-false]
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "m4_ifvaln",
                M4MacroSignature {
                    arg_types: vec![
                        Lit,  // cond
                        Cmds, // [if-true]
                        Cmds, // [if-false]
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "m4_n",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // text
                    ],
                    ret_type: Some(Lit),
                    ..Default::default()
                },
            ),
            (
                "m4_argn",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // n
                        Lit, // [arg]...
                    ],
                    ret_type: Some(Lit),
                    repeat: Some((1, 1)),
                    ..Default::default()
                },
            ), //varargs
            (
                "m4_car",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // arg...
                    ],
                    ret_type: Some(Lit),
                    repeat: Some((0, 0)),
                    ..Default::default()
                },
            ), //varargs
            (
                "m4_cdr",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // arg..
                    ],
                    ret_type: Some(Lit),
                    repeat: Some((0, 0)),
                    ..Default::default()
                },
            ), //varargs
            (
                "m4_for",
                M4MacroSignature {
                    arg_types: vec![
                        Lit,  // var
                        Lit,  // first
                        Lit,  // last
                        Lit,  // [step]
                        Cmds, // expression
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "m4_foreach",
                M4MacroSignature {
                    arg_types: vec![
                        Lit,        // var
                        Arr(Comma), // list
                        Cmds,       // expression
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                // FIXME: just a temporal solution to escape evaluation of m4_foreach
                // see rewite_quotes.rs
                // It's to dumb so will delete soon.
                "M4_FOREACH",
                M4MacroSignature {
                    replaced_by: Some("m4_foreach".into()),
                    ..Default::default()
                },
            ),
            (
                "m4_foreach_w",
                M4MacroSignature {
                    arg_types: vec![
                        Lit,        // var
                        Arr(Blank), // list
                        Cmds,       // expression
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "m4_map",
                M4MacroSignature {
                    arg_types: vec![
                        Lit,        // macro
                        Arr(Comma), // list
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ), //Actually return type is unknown
            (
                "m4_mapall",
                M4MacroSignature {
                    arg_types: vec![
                        Lit,        // macro
                        Arr(Comma), // list
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "m4_map_sep",
                M4MacroSignature {
                    arg_types: vec![
                        Lit,        // macro
                        Lit,        // separator
                        Arr(Comma), // macro
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "m4_mapall_sep",
                M4MacroSignature {
                    arg_types: vec![
                        Lit,        // macro
                        Lit,        // separator
                        Arr(Comma), // list
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "m4_map_args",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // macro
                        Lit, // arg...
                    ],
                    repeat: Some((1, 1)),
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "m4_map_args_pair",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // macro
                        Lit, // [macro-end=macro]
                        Lit, // arg...
                    ],
                    repeat: Some((2, 2)),
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "m4_map_args_sep",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // [pre]
                        Lit, // [post]
                        Lit, // [sep]
                        Lit, // arg...
                    ],
                    repeat: Some((3, 3)),
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "m4_map_args_w",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // string
                        Lit, // [pre]
                        Lit, // [post]
                        Lit, // [sep]
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "m4_shiftn",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // count,
                        Args,
                    ],
                    ret_type: Some(Args),
                    repeat: Some((1, 1)),
                    ..Default::default()
                },
            ), // varargs
            (
                "m4_shift2",
                M4MacroSignature {
                    arg_types: vec![Args],
                    ret_type: Some(Args),
                    ..Default::default()
                },
            ), // varargs
            (
                "m4_shift3",
                M4MacroSignature {
                    arg_types: vec![Args],
                    ret_type: Some(Args),
                    ..Default::default()
                },
            ), // varargs
            (
                "m4_stack_foreach",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // macro
                        Lit, // action
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "m4_stack_foreach_lifo",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // macro
                        Lit, // action
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "m4_stack_foreach_sep",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // macro
                        Lit, // [pre]
                        Lit, // [post]
                        Lit, // [sep]
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "m4_stack_foreach_sep_lifo",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // macro
                        Lit, // [pre]
                        Lit, // [post]
                        Lit, // [sep]
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "m4_apply",
                M4MacroSignature {
                    arg_types: vec![
                        Lit,        // macro
                        Arr(Comma), // list
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "m4_curry",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // macro
                        Lit, // arg...
                    ],
                    repeat: Some((1, 1)),
                    ret_type: Some(Lit),
                    ..Default::default()
                },
            ), // returns Macro
            (
                "m4_do",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // arg, ...
                    ],
                    ret_type: Some(Lit),
                    repeat: Some((0, 0)),
                    ..Default::default()
                },
            ), //varargs
            (
                "m4_dquote",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // arg, ...
                    ],
                    ret_type: Some(Arr(Comma)),
                    repeat: Some((0, 0)),
                    ..Default::default()
                },
            ), // varargs
            (
                "m4_dquote_elt",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // arg, ...
                    ],
                    ret_type: Some(Arr(Comma)),
                    repeat: Some((0, 0)),
                    ..Default::default()
                },
            ), // varargs
            (
                "m4_echo",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // arg, ...
                    ],
                    ret_type: Some(Lit),
                    repeat: Some((0, 0)),
                    ..Default::default()
                },
            ), // varargs
            (
                "m4_expand",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // arg
                    ],
                    ret_type: Some(Lit),
                    ..Default::default()
                },
            ),
            (
                "m4_ignore",
                M4MacroSignature {
                    arg_types: vec![
                        Args, // ...
                    ],
                    ret_type: None,
                    ..Default::default()
                },
            ),
            (
                "m4_make_list",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // arg
                    ],
                    ret_type: Some(Arr(Comma)),
                    repeat: Some((0, 0)),
                    ..Default::default()
                },
            ),
            (
                "m4_quote",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // arg
                    ],
                    ret_type: Some(Arr(Comma)),
                    repeat: Some((0, 0)),
                    ..Default::default()
                },
            ),
            (
                "m4_reverse",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // arg
                    ],
                    ret_type: Some(Arr(Comma)),
                    repeat: Some((0, 0)),
                    ..Default::default()
                },
            ),
            (
                "m4_unquote",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // arg
                    ],
                    ret_type: Some(Arr(Comma)),
                    repeat: Some((0, 0)),
                    ..Default::default()
                },
            ),
            // string manipulation in m4
            (
                "m4_append",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // macro-name
                        Lit, // [stirng]
                        Lit, // [separator]
                    ],
                    ret_type: Some(Lit),
                    ..Default::default()
                },
            ), //cannot define its return type
            (
                "m4_append_uniq",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // macro-name
                        Lit, // strings
                        Lit, // [separator]
                        Lit, // [if-uniq]
                        Lit, // [if-duplicate]
                    ],
                    ret_type: Some(Lit),
                    ..Default::default()
                },
            ),
            (
                "m4_append_uniq_w",
                M4MacroSignature {
                    arg_types: vec![
                        Lit,        // macro-name
                        Arr(Blank), // macro-strings
                    ],
                    ret_type: Some(Lit),
                    ..Default::default()
                },
            ),
            (
                "m4_chomp",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // string
                    ],
                    ret_type: Some(Lit),
                    ..Default::default()
                },
            ),
            (
                "m4_chomp_all",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // string
                    ],
                    ret_type: Some(Lit),
                    ..Default::default()
                },
            ),
            (
                "m4_combine",
                M4MacroSignature {
                    arg_types: vec![
                        Lit,        // [separator]
                        Arr(Comma), // prefix-list
                        Lit,        // [infix]
                        Lit,        // [suffix-1]
                    ],
                    ret_type: Some(Lit),
                    ..Default::default()
                },
            ),
            (
                "m4_escape",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // string
                    ],
                    ret_type: Some(Lit),
                    ..Default::default()
                },
            ),
            (
                "m4_flatten",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // string
                    ],
                    ret_type: Some(Lit),
                    ..Default::default()
                },
            ),
            (
                "m4_join",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // [separator]
                        Lit, // args
                    ],
                    repeat: Some((1, 1)),
                    ret_type: Some(Lit),
                    ..Default::default()
                },
            ), //varargs
            (
                "m4_joinall",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // [separator]
                        Lit, // args
                    ],
                    repeat: Some((1, 1)),
                    ret_type: Some(Lit),
                    ..Default::default()
                },
            ), //varargs
            (
                "m4_newline",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // text
                    ],
                    ret_type: Some(Lit),
                    ..Default::default()
                },
            ),
            (
                "m4_normalize",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // string
                    ],
                    ret_type: Some(Lit),
                    ..Default::default()
                },
            ),
            (
                "m4_re_escape",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // string
                    ],
                    ret_type: Some(Lit),
                    ..Default::default()
                },
            ),
            (
                "m4_split",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // string
                        Lit, // [regexp=[\t ]+]
                    ],
                    ret_type: Some(Arr(Comma)),
                    ..Default::default()
                },
            ),
            (
                "m4_strip",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // string
                    ],
                    ret_type: Some(Lit),
                    ..Default::default()
                },
            ),
            (
                "m4_text_box",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // message
                        Lit, // [frame=-1]
                    ],
                    ret_type: Some(Lit),
                    ..Default::default()
                },
            ),
            (
                "m4_text_wrap",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // string
                        Lit, // [prefix]
                        Lit, // [prefix1=prefix]
                        Lit, // [width=79]
                    ],
                    ret_type: Some(Lit),
                    ..Default::default()
                },
            ),
            (
                "m4_tolower",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // string
                    ],
                    ret_type: Some(Lit),
                    ..Default::default()
                },
            ),
            (
                "m4_toupper",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // string
                    ],
                    ret_type: Some(Lit),
                    ..Default::default()
                },
            ),
            (
                "m4_cmp",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // expr-1
                        Lit, // expr-2
                    ],
                    ret_type: Some(Lit),
                    ..Default::default()
                },
            ),
            (
                "m4_list_cmp",
                M4MacroSignature {
                    arg_types: vec![
                        Arr(Comma), // list-1
                        Arr(Comma), // list-2
                    ],
                    ret_type: Some(Lit),
                    ..Default::default()
                },
            ),
            (
                "m4_max",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // arg,...
                    ],
                    ret_type: Some(Lit),
                    repeat: Some((0, 0)),
                    ..Default::default()
                },
            ), //varargs
            (
                "m4_min",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // arg,...
                    ],
                    ret_type: Some(Lit),
                    repeat: Some((0, 0)),
                    ..Default::default()
                },
            ), //varargs
            (
                "m4_sign",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // expr
                    ],
                    ret_type: Some(Lit),
                    ..Default::default()
                },
            ),
            (
                "m4_version_compare",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // version-1
                        Lit, // version-2
                    ],
                    ret_type: Some(Lit),
                    ..Default::default()
                },
            ),
            (
                "m4_version_prereq",
                M4MacroSignature {
                    arg_types: vec![
                        Lit,  // version
                        Cmds, // [if-new-enough]
                        Cmds, // [if-old=m4_fatal]
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            // set manipulations in m4
            (
                "m4_set_add",
                M4MacroSignature {
                    arg_types: vec![
                        Lit,  // set
                        Lit,  // value
                        Cmds, // [if-uniq]
                        Cmds, // [if-dup]
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "m4_set_add_all",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // set
                        Lit, // value...
                    ],
                    repeat: Some((1, 1)),
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "m4_set_contains",
                M4MacroSignature {
                    arg_types: vec![
                        Lit,  //  set
                        Lit,  // value
                        Cmds, // [if-present]
                        Cmds, // [if-absent]
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "m4_set_contents",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // set
                        Lit, // [sep]
                    ],
                    ret_type: Some(Lit),
                    ..Default::default()
                },
            ),
            (
                "m4_set_dump",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // set
                        Lit, // [sep]
                    ],
                    ret_type: Some(Lit),
                    ..Default::default()
                },
            ),
            (
                "m4_set_delete",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // set
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "m4_set_difference",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // set-a
                        Lit, // set-b
                    ],
                    ret_type: Some(Arr(Comma)),
                    ..Default::default()
                },
            ),
            (
                "m4_set_intersection",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // set-a
                        Lit, // set-b
                    ],
                    ret_type: Some(Arr(Comma)),
                    ..Default::default()
                },
            ),
            (
                "m4_set_union",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // set-a
                        Lit, // set-b
                    ],
                    ret_type: Some(Arr(Comma)),
                    ..Default::default()
                },
            ),
            (
                "m4_set_empty",
                M4MacroSignature {
                    arg_types: vec![
                        Lit,  // set
                        Cmds, // [if-empty]
                        Cmds, // [if-elements]
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "m4_set_foreach",
                M4MacroSignature {
                    arg_types: vec![
                        Lit,  // set
                        Lit,  // variable
                        Cmds, // action
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "m4_set_list",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // set
                    ],
                    ret_type: Some(Arr(Comma)),
                    ..Default::default()
                },
            ),
            (
                "m4_set_listc",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // set
                    ],
                    ret_type: Some(Arr(Comma)),
                    ..Default::default()
                },
            ),
            (
                "m4_set_map",
                M4MacroSignature {
                    arg_types: vec![
                        Lit,  // set
                        Cmds, // action
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "m4_set_map_sep",
                M4MacroSignature {
                    arg_types: vec![
                        Lit,  // set
                        Cmds, // [pre]
                        Cmds, // [post]
                        Lit,  // [sep]
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "m4_set_size",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // set
                    ],
                    ret_type: Some(Lit),
                    ..Default::default()
                },
            ),
            // pattern allow/forbid
            (
                "m4_pattern_allow",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // pattern
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "m4_pattern_forbid",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // pattern
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            // Common shell constructs
            (
                "AS_BOX",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // text
                        Lit, // [char=-1]
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AS_CASE",
                M4MacroSignature {
                    arg_types: vec![
                        Word, // word
                        // maybe this needs to support multiple words concatenation
                        // or, we have to define a new arg type `Pattern`.
                        Word, // [pattern1]
                        Cmds, // [if-matched1]
                        Cmds, // [default]
                    ],
                    ret_type: Some(Cmds),
                    repeat: Some((1, 2)),
                    ..Default::default()
                },
            ), //varargs
            (
                "AS_DIRNAME",
                M4MacroSignature {
                    arg_types: vec![
                        Path(None), // file-name
                    ],
                    ret_type: Some(Lit),
                    ..Default::default()
                },
            ),
            (
                "AS_ECHO",
                M4MacroSignature {
                    arg_types: vec![
                        Word, // word
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AS_ECHO_N",
                M4MacroSignature {
                    arg_types: vec![
                        Word, // word
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AS_ESCAPE",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // string
                        Lit, // [chars=`\"$]
                    ],
                    ret_type: Some(Lit),
                    ..Default::default()
                },
            ),
            (
                "AS_EXECUTABLE_P",
                M4MacroSignature {
                    arg_types: vec![
                        Path(None), // file
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AS_EXIT",
                M4MacroSignature {
                    arg_types: vec![
                        Word, // [status=$?]
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AS_IF",
                M4MacroSignature {
                    arg_types: vec![
                        Cmds, // test1
                        Cmds, // [run-if-true1]
                        Cmds, // [run-if-false]
                    ],
                    ret_type: Some(Cmds),
                    repeat: Some((0, 1)),
                    ..Default::default()
                },
            ), // varargs
            (
                "AS_MKDIR_P",
                M4MacroSignature {
                    arg_types: vec![
                        Path(None), // file-name
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AS_SET_STATUS",
                M4MacroSignature {
                    arg_types: vec![
                        Word, // status
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AS_TR_CPP",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // expression
                    ],
                    ret_type: Some(CPPSymbol),
                    ..Default::default()
                },
            ),
            (
                "AS_TR_SH",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // expression
                    ],
                    ret_type: Some(Word),
                    ..Default::default()
                },
            ),
            (
                "AS_SET_CATFILE",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // var
                        Lit, // dir
                        Lit, // file
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AS_UNSET",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // var
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AS_VERSION_COMPARE",
                M4MacroSignature {
                    arg_types: vec![
                        Lit,  // version-1
                        Lit,  // version-2
                        Cmds, // [action-if-less]
                        Cmds, // [action-if-equal]
                        Cmds, // [action-if-greater]
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            // Support for indirect variable names
            (
                "AS_LITERAL_IF",
                M4MacroSignature {
                    arg_types: vec![
                        Lit,  // expressio
                        Cmds, // [if-literal]
                        Cmds, // [if-not]
                        Cmds, // [if-simple-ref=if-not]
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AS_LITERAL_WORD_IF",
                M4MacroSignature {
                    arg_types: vec![
                        Lit,  // expression
                        Cmds, // [if-literal]
                        Cmds, // [if-not]
                        Cmds, // [if-simple-ref]
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AS_VAR_APPEND",
                M4MacroSignature {
                    arg_types: vec![
                        VarName(Some(VarAttrs::internal()), None), // var
                        // In most cases, TEXT is a shell word except a leading
                        // whitespace to deliminate it from VAR.
                        Word, // text
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AS_VAR_ARITH",
                M4MacroSignature {
                    arg_types: vec![
                        VarName(Some(VarAttrs::internal()), None), // var
                        Lit,                                       // expression
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AS_VAR_COPY",
                M4MacroSignature {
                    arg_types: vec![
                        VarName(Some(VarAttrs::internal()), None), // dest
                        VarName(Some(VarAttrs::internal()), None), // source
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AS_VAR_IF",
                M4MacroSignature {
                    arg_types: vec![
                        VarName(Some(VarAttrs::internal()), None), // var
                        Word,                                      // word
                        Cmds,                                      // [if-equal]
                        Cmds,                                      // [if-not-equal]
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AS_VAR_PUSHDEF",
                // AS_VAR_PUSHDEF/AS_VAR_POPDEF are mainly used for variable aliasing.
                // Especially in a macro body where a variable reference is crafted
                // using an argument's value.
                M4MacroSignature {
                    arg_types: vec![
                        VarName(Some(VarAttrs::internal()), None), // var
                        VarName(Some(VarAttrs::internal()), None), // value
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AS_VAR_POPDEF",
                M4MacroSignature {
                    arg_types: vec![
                        VarName(Some(VarAttrs::internal()), None), // var
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AS_VAR_SET",
                M4MacroSignature {
                    arg_types: vec![
                        VarName(Some(VarAttrs::internal()), None), // var
                        Word,                                      // value
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AS_VAR_SET_IF",
                M4MacroSignature {
                    arg_types: vec![
                        VarName(Some(VarAttrs::internal()), None), // var
                        Cmds,                                      // [if-set]
                        Cmds,                                      // [if-undef]
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AS_VAR_TEST_SET",
                M4MacroSignature {
                    arg_types: vec![
                        VarName(Some(VarAttrs::internal()), None), // var
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            // Initialization macros
            (
                "AS_BOURNE_COMPATIBLE",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AS_INIT",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_env("SHELL"), Var::define_env("LC_ALL")]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AS_INIT_GENERATED",
                M4MacroSignature {
                    arg_types: vec![
                        Path(None), // file
                        Lit,        // comment
                    ],
                    ret_type: Some(Cmds),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AS_LINENO_PREPARE",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec!["LINENO".into()]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AS_UNAME",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        // by _AS_PATH_WALK
                        "as_dir".into(),
                    ]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AS_ME_PREPARE",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec!["as_me".into()]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AS_TMPDIR",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // prefix
                        Lit, // [dir=${tmpdir:=/tmp}]
                    ],
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec!["tmp".into()]),
                    ..Default::default()
                },
            ),
            (
                "AS_SHELL_SANITIZE", // obsolete. AS_INIT already invokes it.
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec!["LC_ALL".into()]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            // File descriptor macros
            (
                "AS_MESSAGE_FD",
                M4MacroSignature {
                    ret_type: Some(Lit),
                    ..Default::default()
                },
            ),
            (
                "AS_MESSAGE_LOG_FD",
                M4MacroSignature {
                    ret_type: Some(Lit),
                    ..Default::default()
                },
            ),
            (
                "AC_FD_CC",
                M4MacroSignature {
                    ret_type: Some(Lit),
                    ..Default::default()
                },
            ), // deprecated
            (
                "AS_ORIGINAL_STDIN_FD",
                M4MacroSignature {
                    ret_type: Some(Lit),
                    ..Default::default()
                },
            ),
            // Macro definitions
            (
                "AC_DEFUN",
                M4MacroSignature {
                    arg_types: vec![
                        Lit,  // name
                        Body, // [body]
                    ],
                    ret_type: Some(Def),
                    ..Default::default()
                },
            ), // The command parsing is delayed till expandsion
            // Prerequisite macros
            (
                "AC_REQUIRE",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // name
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "m4_require",
                M4MacroSignature {
                    replaced_by: Some("AC_REQUIRE".into()),
                    ..Default::default()
                },
            ),
            (
                "AC_BEFORE",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, //
                        Lit, //
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            // One-shot macros
            (
                "AC_DEFUN_ONCE",
                M4MacroSignature {
                    arg_types: vec![
                        Lit,  // name
                        Body, // [body]
                    ],
                    ret_type: Some(Def),
                    ..Default::default()
                },
            ),
            // Getting the canonical system type
            (
                "AC_CANONICAL_BUILD",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        Var::define_precious("build"),
                        Var::define_precious("build_cpu"),
                        Var::define_precious("build_vendor"),
                        Var::define_precious("build_os"),
                        Var::define_precious("build_alias"),
                    ]),
                    paths: Some(vec!["config.sub".into(), "config.guess".into()]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AC_CANONICAL_HOST",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        Var::define_precious("host"),
                        Var::define_precious("host_cpu"),
                        Var::define_precious("host_vendor"),
                        Var::define_precious("host_os"),
                        Var::define_precious("host_alias"),
                    ]),
                    paths: Some(vec!["config.sub".into(), "config.guess".into()]),
                    require: Some(vec!["AC_CANONICAL_BUILD".into()]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AC_CANONICAL_TARGET",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        Var::define_precious("target"),
                        Var::define_precious("target_cpu"),
                        Var::define_precious("target_vendor"),
                        Var::define_precious("target_os"),
                        Var::define_precious("target_alias"),
                    ]),
                    paths: Some(vec!["config.sub".into(), "config.guess".into()]),
                    require: Some(vec!["AC_CANONICAL_HOST".into()]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            // Site configuration
            (
                "AC_PRESERVE_HELP_ORDER",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AC_ARG_WITH",
                M4MacroSignature {
                    arg_types: vec![
                        // actually the variable with the package name itself won't be defined.
                        VarName(
                            Some(VarAttrs::internal()),
                            Some(&|s| {
                                vec![
                                    // TODO: we are interested in what values could be assigned to the
                                    // option variable. we might add extra information for it.
                                    ExVar(
                                        VarAttrs::input(),
                                        format!("with_{}", sanitize_shell_name(s)),
                                        None,
                                    ),
                                ]
                            }),
                        ), // package
                        Lit,  // help-string (can be a call to AS_HELP_STRING)
                        Cmds, // [actio-if-given]
                        Cmds, // [action-if-not-given]
                    ],
                    shell_vars: Some(vec!["withval".into()]),
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_ARG_ENABLE",
                M4MacroSignature {
                    arg_types: vec![
                        VarName(
                            Some(VarAttrs::internal()),
                            Some(&|s| {
                                vec![ExVar(
                                    VarAttrs::input(),
                                    format!("enable_{}", sanitize_shell_name(s)),
                                    None,
                                )]
                            }),
                        ), // feature
                        Lit,  // help-string
                        Cmds, // [action-if-given]
                        Cmds, // [action-if-not-given]
                    ],
                    shell_vars: Some(vec!["enableval".into()]),
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AS_HELP_STRING",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // left-hand-side
                        Lit, // right-hand-side
                    ],
                    ret_type: Some(Lit),
                    ..Default::default()
                },
            ),
            (
                "AC_DISABLE_OPTION_CHECKING",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_ARG_PROGRAM",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_output("program_transform_name")]),
                    ..Default::default()
                },
            ),
            // Generating test suites with Autotest
            (
                "AT_INIT",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // name
                    ],
                    ret_type: Some(Cmds),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AT_COPYRIGHT",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // copyright-notice
                    ],
                    ret_type: Some(Cmds),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AT_ARG_OPTION",
                M4MacroSignature {
                    arg_types: vec![
                        Arr(Blank), // options
                        Lit,        // help-text
                        Cmds,       // [action-if-given]
                        Cmds,       // [action-if-not-given]
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AT_ARG_OPTION_ARG",
                M4MacroSignature {
                    arg_types: vec![
                        Arr(Blank), // options
                        Lit,        // help-text
                        Cmds,       // [action-if-given]
                        Cmds,       // [action-if-not-given]
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AT_COLOR_TESTS",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AT_TESTED",
                M4MacroSignature {
                    arg_types: vec![
                        Arr(Blank), // executables
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AT_PREPARE_TESTS",
                M4MacroSignature {
                    arg_types: vec![
                        Cmds, // shell-code
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AT_PREPARE_EACH_TEST",
                M4MacroSignature {
                    arg_types: vec![
                        Cmds, // shell-code
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AT_TEST_HELPER_FN",
                M4MacroSignature {
                    arg_types: vec![
                        Lit,  // name
                        Lit,  // args
                        Lit,  // description
                        Cmds, // code
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AT_BANNER",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // test-category-name
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AT_SETUP",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // test-group-name
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AT_KEYWORDS",
                M4MacroSignature {
                    arg_types: vec![
                        Arr(Blank), // keywords
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AT_CAPTURE_FILE",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // file
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AT_FAIL_IF",
                M4MacroSignature {
                    arg_types: vec![
                        Cmds, // shell-condition
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AT_SKIP_IF",
                M4MacroSignature {
                    arg_types: vec![
                        Cmds, // shell-condition
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AT_XFAIL_IF",
                M4MacroSignature {
                    arg_types: vec![
                        Cmds, // shell-condition
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AT_CLEANUP",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AT_DATA",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // file
                        Lit, // contents
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AT_DATA_UNQUOTED",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // file
                        Lit, // contents
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AT_CHECK",
                M4MacroSignature {
                    arg_types: vec![
                        Cmds, // commands
                        Lit,  // [status=0]
                        Lit,  // [stdout]
                        Lit,  // [stderr]
                        Cmds, // [run-if-fail]
                        Cmds, // [run-if-pass]
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AT_CHECK_UNQUOTED",
                M4MacroSignature {
                    arg_types: vec![
                        Cmds, // commands
                        Lit,  // [status=0]
                        Lit,  // [stdout]
                        Lit,  // [stderr]
                        Cmds, // [run-if-fail]
                        Cmds, // [run-if-pass]
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AT_CHECK_EUNIT",
                M4MacroSignature {
                    arg_types: vec![
                        Lit,  // module
                        Lit,  // test-spec
                        Lit,  // [erlflags]
                        Cmds, // [run-if-fail]
                        Cmds, // [run-if-pass]
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_CONFIG_TESTDIR",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // directory
                        Lit, // [test-path=directory]
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            // pkg.m4 (PKG_*) macros
            (
                "PKG_PREREQ",
                M4MacroSignature {
                    arg_types: vec![
                        Word, // min-version
                    ],
                    ret_type: Some(Cmds),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "PKG_PROG_PKG_CONFIG",
                M4MacroSignature {
                    arg_types: vec![
                        Word, // min-version
                    ],
                    shell_vars: Some(vec![
                        Var::define_precious("PKG_CONFIG").with_value("pkg-config"),
                        Var::define_precious("PKG_CONFIG_PATH").with_value(""),
                        Var::define_precious("PKG_CONFIG_LIBDIR").with_value(""),
                    ]),
                    ret_type: Some(Cmds),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "PKG_CHECK_MODULES",
                M4MacroSignature {
                    arg_types: vec![
                        VarName(
                            None,
                            Some(&|s| {
                                // variable-prefix
                                vec![
                                    ExVar(VarAttrs::output(), format!("{}_CFLAGS", s), None),
                                    ExVar(VarAttrs::output(), format!("{}_LIBS", s), None),
                                ]
                            }),
                        ),
                        Arr(Blank),
                        Cmds, // [action-if-found]
                        Cmds, // [action-if-not-found]
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "PKG_CHECK_MODULES_STATIC",
                M4MacroSignature {
                    arg_types: vec![
                        VarName(
                            None,
                            Some(&|s| {
                                // variable-prefix
                                vec![
                                    ExVar(VarAttrs::output(), format!("{}_CFLAGS", s), None),
                                    ExVar(VarAttrs::output(), format!("{}_LIBS", s), None),
                                ]
                            }),
                        ),
                        Arr(Blank),
                        Cmds, // [action-if-found]
                        Cmds, // [action-if-not-found]
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "PKG_INSTALLDIR",
                M4MacroSignature {
                    arg_types: vec![
                        Word, // directory
                    ],
                    shell_vars: Some(vec![
                        // $pkgconfigdir is set to $libdir/pkgconfig by default
                        Var::define_precious("pkgconfigdir"),
                        Var::reference("libdir"),
                    ]),
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "PKG_NOARCH_INSTALLDIR",
                M4MacroSignature {
                    arg_types: vec![
                        Word, // directory
                    ],
                    shell_vars: Some(vec![
                        // $noarch_pkgconfigdir is set to $datadir/pkgconfig by default
                        Var::define_precious("noarch_pkgconfigdir"),
                        Var::reference("datadir"),
                    ]),
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "PKG_CHECK_VAR",
                M4MacroSignature {
                    arg_types: vec![
                        VarName(Some(VarAttrs::output()), None), // variable
                        Word,                                    // module
                        Word,                                    // config-variable
                        Cmds,                                    // [action-if-found]
                        Cmds,                                    // [action-if-not-found]
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "PKG_WITH_MODULES",
                // PKG_WITH_MODULES = AC_ARG_WITH + PKG_CHECK_MODULES
                M4MacroSignature {
                    arg_types: vec![
                        VarName(
                            None,
                            Some(&|s| {
                                // variable-prefix
                                vec![
                                    ExVar(VarAttrs::output(), format!("{}_CFLAGS", s), None),
                                    ExVar(VarAttrs::output(), format!("{}_LIBS", s), None),
                                    // argument
                                    ExVar(VarAttrs::input(), format!("with_{}", s), None),
                                ]
                            }),
                        ),
                        Arr(Blank),
                        Cmds, // [action-if-found]
                        Cmds, // [action-if-not-found]
                        Lit,  // [description]
                        Word, // [default]
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "PKG_HAVE_WITH_MODULES",
                // PKG_HAVE_WITH_MODULES = AC_ARG_WITH + PKG_CHECK_MODULES + AM_CONDITIONAL
                M4MacroSignature {
                    arg_types: vec![
                        VarName(
                            None,
                            Some(&|s| {
                                // variable-prefix
                                vec![
                                    ExVar(VarAttrs::output(), format!("{}_CFLAGS", s), None),
                                    ExVar(VarAttrs::output(), format!("{}_LIBS", s), None),
                                    // argument & conditional
                                    ExVar(VarAttrs::input(), format!("with_{}", s), None),
                                    ExCond(s.into()),
                                ]
                            }),
                        ),
                        Arr(Blank),
                        Cmds, // [action-if-found]
                        Cmds, // [action-if-not-found]
                        Lit,  // [description]
                        Word, // [default]
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "PKG_HAVE_DEFINE_WITH_MODULES",
                // PKG_HAVE_WITH_MODULES =
                //  AC_ARG_WITH + PKG_CHECK_MODULES + AM_CONDITIONAL + AC_DEFINE
                M4MacroSignature {
                    arg_types: vec![
                        VarName(
                            None,
                            Some(&|s| {
                                // variable-prefix
                                vec![
                                    ExVar(VarAttrs::output(), format!("{}_CFLAGS", s), None),
                                    ExVar(VarAttrs::output(), format!("{}_LIBS", s), None),
                                    // argument & conditional & cpp symbol
                                    ExVar(VarAttrs::input(), format!("with_{}", s), None),
                                    ExCond(s.into()),
                                    ExCPP(s.into(), None),
                                ]
                            }),
                        ),
                        Arr(Blank),
                        Cmds, // [action-if-found]
                        Cmds, // [action-if-not-found]
                        Lit,  // [description]
                        Word, // [default]
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            // libtool (LT_*) macros
            (
                "LT_PREREQ",
                M4MacroSignature {
                    arg_types: vec![
                        Word, // version
                    ],
                    ret_type: Some(Cmds),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "LT_INIT",
                M4MacroSignature {
                    arg_types: vec![
                        Arr(Blank), // options
                    ],
                    shell_vars: Some(vec![
                        Var::define_precious("LT_SYS_LIBRARY_PATH").with_value(""),
                        Var::define_input("with_gnu_ld").yes(),
                        Var::define_input("with_tags").with_value(""),
                        Var::define_input("with_pic").yes(),
                        Var::define_input("enable_libtool_lock").no(),
                        Var::define_input("enable_static").yes(),
                        Var::define_input("enable_shared").yes(),
                        Var::define_precious("sysroot").with_value(""),
                        "library_names_spec".into(),
                        // by _LT_COMPILER_PIC
                        Var::define_internal("lt_prog_compiler_wl").with_value("-Wl,"),
                        Var::define_internal("lt_prog_compiler_pic").with_value("-fPIC"),
                        Var::define_internal("lt_prog_compiler_static").with_value(""),
                        Var::define_internal("lt_prog_compiler_can_build_shared").yes(),
                        Var::define_internal("lt_prog_compiler_no_builtin_flag").with_value(""),
                        // by _LT_LANG_C_CONFIG
                        Var::define_internal("objext").with_value("o"),
                        Var::define_internal("ac_ext").with_value("c"),
                        Var::define_internal("allow_undefined_flag").with_value(""),
                        // by _LT_TAGVAR
                        Var::define_internal("always_export_symbols").no(),
                        "archive_cmds".into(),
                        "archive_cmds_need_lc".into(),
                        "archive_expsym_cmds".into(),
                        "compiler".into(),
                        "compiler_lib_search_dirs".into(),
                        "compiler_lib_search_path".into(),
                        "compiler_needs_object".into(),
                        "enable_shared_with_static_runtimes".into(),
                        "exclude_expsyms".into(),
                        "export_dynamic_flag_spec".into(),
                        "export_symbols_cmds".into(),
                        "file_list_spec".into(),
                        Var::define_internal("hardcode_action").with_value("immediate"),
                        Var::define_internal("hardcode_automatic").yes(),
                        "hardcode_direct_absolute".into(),
                        "hardcode_direct".into(),
                        "hardcode_libdir_flag_spec".into(),
                        "hardcode_libdir_separator".into(),
                        Var::define_internal("hardcode_minus_L").no(),
                        "hardcode_shlibpath_var".into(),
                        "include_expsyms".into(),
                        Var::define_internal("inherit_rpath").yes(),
                        Var::define_internal("ld_shlibs").yes(),
                        Var::define_internal("link_all_deplibs").yes(),
                        "lt_cv_prog_compiler__b".into(),
                        "lt_cv_prog_compiler_c_o".into(),
                        "lt_cv_prog_compiler_pic".into(),
                        "lt_cv_prog_compiler_pic_works".into(),
                        "lt_cv_prog_compiler_static_works".into(),
                        "module_cmds".into(),
                        "module_expsym_cmds".into(),
                        "no_undefined_flag".into(),
                        "objext".into(),
                        "old_archive_cmds".into(),
                        "old_archive_from_expsyms_cmds".into(),
                        "old_archive_from_new_cmds".into(),
                        "old_postinstall_cmds".into(),
                        "postdep_objects".into(),
                        "postdeps".into(),
                        "postlink_cmds".into(),
                        "predep_objects".into(),
                        "predeps".into(),
                        "prelink_cmds".into(),
                        "reload_cmds".into(),
                        "reload_flag".into(),
                        Var::define_internal("runpath_var").with_value("LD_RUN_PATH"),
                        Var::define_internal("thread_safe_flag_spec").with_value("-pthread"),
                        "whole_archive_flag_spec".into(),
                    ]),
                    ret_type: Some(Cmds),
                    require: Some(vec![
                        "LT_PATH_LD".into(),
                        "LT_PATH_NM".into(),
                        "AC_PROG_LN_S".into(),
                    ]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AC_PROG_LIBTOOL", // obsolete macro, replaced by LT_INIT
                M4MacroSignature {
                    replaced_by: Some("LT_INIT".into()),
                    ..Default::default()
                },
            ),
            (
                "AM_PROG_LIBTOOL", // obsolete macro, replaced by LT_INIT
                M4MacroSignature {
                    replaced_by: Some("LT_INIT".into()),
                    ..Default::default()
                },
            ),
            (
                "AC_DISABLE_SHARED", // obsolete macro, replaced by LT_INIT
                M4MacroSignature {
                    replaced_by: Some("LT_INIT".into()),
                    ..Default::default()
                },
            ),
            (
                "LT_LANG",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // language
                    ],
                    ..Default::default()
                },
            ),
            (
                "AC_LIBTOOL_DLOPEN",
                // deprecated. replaced by 'dlopen' option of LT_INIT.
                M4MacroSignature {
                    replaced_by: Some("LT_INIT".into()),
                    ..Default::default()
                },
            ),
            (
                "AC_LIBTOOL_WIN32_DLL",
                // deprecated. replaced by 'win32-dll' option of LT_INIT.
                M4MacroSignature {
                    replaced_by: Some("LT_INIT".into()),
                    ..Default::default()
                },
            ),
            (
                "AC_LIBTOOL_PROG_COMPILER_PIC",
                // deprecated. replaced by _LT_COMPILER_PIC in LT_INIT.
                M4MacroSignature {
                    replaced_by: Some("LT_INIT".into()),
                    ..Default::default()
                },
            ),
            (
                "AC_DISABLE_SHARED",
                // change the default libtool's behavior to disable shared libraries.
                M4MacroSignature {
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AM_DISABLE_SHARED",
                // deprecated. replaced by AC_DISABLE_SHARED
                M4MacroSignature {
                    replaced_by: Some("AC_DISABLE_SHARED".into()),
                    ..Default::default()
                },
            ),
            (
                "AC_ENABLE_SHARED",
                // change the default libtool's behavior to enable shared libraries.
                M4MacroSignature {
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AM_ENABLE_SHARED",
                // deprecated. replaced by AC_ENABLE_SHARED
                M4MacroSignature {
                    replaced_by: Some("AC_ENABLE_SHARED".into()),
                    ..Default::default()
                },
            ),
            (
                "AC_DISABLE_STATIC",
                // change the default libtool's behavior to disable static libraries.
                M4MacroSignature {
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AM_DISABLE_STATIC",
                // deprecated. replaced by AC_DISABLE_STATIC
                M4MacroSignature {
                    replaced_by: Some("AC_DISABLE_STATIC".into()),
                    ..Default::default()
                },
            ),
            (
                "AC_ENABLE_STATIC",
                // change the default libtool's behavior to disable static libraries.
                M4MacroSignature {
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AM_ENABLE_STATIC",
                // deprecated. replaced by AC_ENABLE_STATIC
                M4MacroSignature {
                    replaced_by: Some("AC_ENABLE_STATIC".into()),
                    ..Default::default()
                },
            ),
            (
                "LT_OUTPUT",
                M4MacroSignature {
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "LT_CMD_MAX_LEN",
                M4MacroSignature {
                    shell_vars: Some(vec!["max_cmd_len".into()]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "LT_FUNC_DLSYM_USCORE",
                M4MacroSignature {
                    cpp_symbols: Some(vec![CPP::unset("DLSYM_USCORE")]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "LT_LIB_M",
                M4MacroSignature {
                    shell_vars: Some(vec![
                        Var::define_output("LIBM"),
                        Var::append("LIBS", Output),
                    ]),
                    cpp_symbols: Some(vec![CPP::set("HAVE_LIBM"), CPP::unset("HAVE_LIBMW")]),
                    require: Some(vec!["AC_CANONICAL_HOST".into()]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AC_CHECK_LIBM",
                M4MacroSignature {
                    replaced_by: Some("LT_LIB_M".into()),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "LT_LIB_DLLOAD",
                M4MacroSignature {
                    shell_vars: Some(vec![
                        Var::define_output("LT_DLLOADERS"),
                        Var::define_output("LIBADD_SHL_LOAD"),
                        Var::append("LIBS", Output),
                    ]),
                    cpp_symbols: Some(vec![
                        "HAVE_LIBDL".into(),    // linux
                        "HAVE_SHL_LOAD".into(), // hp-ux
                        "HAVE_DYLD".into(),     // mac
                        CPP::unset("HAVE_DLD"), // obsolete
                    ]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "LT_PATH_LD",
                M4MacroSignature {
                    shell_vars: Some(vec![
                        Var::define_input("with_gnu_ld"),
                        Var::define_output("LD"),
                        "lt_cv_path_LD".into(),
                    ]),
                    require: Some(vec![
                        "AC_PROG_CC".into(),
                        "AC_CANONICAL_HOST".into(),
                        "AC_CANONICAL_BUILD".into(),
                    ]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AC_PROG_LD", // obsolete macro
                M4MacroSignature {
                    replaced_by: Some("LT_PATH_LD".into()),
                    ..Default::default()
                },
            ),
            (
                "LT_PATH_NM",
                M4MacroSignature {
                    shell_vars: Some(vec![
                        Var::define_output("NM"),
                        Var::define_output("DUMPBIN"),
                        "lt_cv_path_NM".into(),
                        "lt_cv_path_NM".into(),
                        "lt_cv_nm_interface".into(),
                    ]),
                    require: Some(vec!["AC_PROG_CC".into()]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AC_PROG_NM", // obsolete macro
                M4MacroSignature {
                    replaced_by: Some("LT_PATH_NM".into()),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "LT_SYS_DLOPEN_SELF",
                M4MacroSignature {
                    shell_vars: Some(vec![
                        "enable_dlopen".into(),
                        "enable_dlopen_self".into(),
                        "enable_dlopen_self_static".into(),
                        "lt_cv_dlopen_self".into(),
                        "lt_cv_dlopen_self_static".into(),
                        Var::append("LIBS", Output),
                    ]),
                    cpp_symbols: Some(vec![
                        CPP::set("HAVE_DLOPEN"),
                        CPP::set("HAVE_DLOPEN_SELF"),
                        CPP::set("HAVE_DLFCN_H"),
                    ]),
                    paths: Some(vec!["dlfcn.h".into()]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AC_LIBTOOL_DLOPEN_SELF", // obsolete macro
                M4MacroSignature {
                    replaced_by: Some("LT_SYS_DLOPEN_SELF".into()),
                    ..Default::default()
                },
            ),
            (
                "LT_SYS_DLOPEN_DEPLIBS",
                M4MacroSignature {
                    shell_vars: Some(vec![Var::define_internal("lt_cv_sys_dlopen_deplibs").no()]),
                    cpp_symbols: Some(vec![CPP::unset("LTDL_DLOPEN_DEPLIBS")]),
                    require: Some(vec!["AC_CANONICAL_HOST".into()]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AC_LTDL_SYS_DLOPEN_DEPLIBS", // obsolete macro
                M4MacroSignature {
                    replaced_by: Some("LT_SYS_DLOPEN_DEPLIBS".into()),
                    ..Default::default()
                },
            ),
            (
                "LT_SYS_DLSEARCH_PATH",
                M4MacroSignature {
                    shell_vars: Some(vec![
                        Var::define_internal("lt_cv_sys_dlsearch_path").yes(),
                        Var::define_internal("sys_dlsearch_path").with_value("/lib:/usr/lib"),
                    ]),
                    cpp_symbols: Some(vec![CPP::with_value("LTDL_DLSERCH_PATH", "/lib:/usr/lib")]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AC_LTDL_SYSSEARCHPATH", // obsolete macro
                M4MacroSignature {
                    replaced_by: Some("LT_SYS_DLSEARCH_PATH".into()),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "LT_SYS_MODULE_EXIT",
                M4MacroSignature {
                    shell_vars: Some(vec![
                        "libltdl_cv_shlibext".into(),
                        "libltdl_cv_shext".into(),
                        "shared_archive_member_spec".into(),
                    ]),
                    cpp_symbols: Some(vec![
                        "LT_MODULE_EXT".into(),
                        "LT_SHARED_EXT".into(),
                        "LT_SHARED_LIB_MEMBER".into(),
                    ]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AC_LTDL_SHLIBEXT", // obsolete macro
                M4MacroSignature {
                    replaced_by: Some("LT_SYS_MODULE_EXT".into()),
                    ..Default::default()
                },
            ),
            (
                "LT_SYS_SYMBOL_USCORE",
                M4MacroSignature {
                    shell_vars: Some(vec![
                        "lt_cv_sys_symbol_underscore".into(),
                        Var::define_output("sys_symbol_underscore"),
                    ]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AC_LTDL_SYMBOL_USCORE", // obsolete macro
                M4MacroSignature {
                    replaced_by: Some("LT_SYS_SYMBOL_USCORE".into()),
                    ..Default::default()
                },
            ),
            // autoconf archive
            (
                "AX_PREFIX_CONFIG_H",
                M4MacroSignature {
                    arg_types: vec![
                        Path(None), // output-header
                        Lit,        // prefix
                        Path(None), // orig-header
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AX_COMPILER_FLAGS",
                M4MacroSignature {
                    arg_types: vec![
                        VarName(Some(VarAttrs::read(None)), None), // cflags-variable
                        VarName(Some(VarAttrs::read(None)), None), // ldflags-variable
                        Word,                                      // [is-release]
                        Arr(Blank),                                // [extra-base-cflags]
                        Arr(Blank),                                // [extra-yes-cflags]
                        Lit,                                       // [unused]
                        Lit,                                       // [unused]
                        Lit,                                       // [unused]
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AX_COMPILE_CHECK_SIZEOF",
                M4MacroSignature {
                    arg_types: vec![
                        Type(Some(&|s| {
                            // type
                            vec![ExCPP(format!("SIZE_OF_{}", sanitize_c_name(s)), None)]
                        })),
                        Prog, // headers
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AX_IS_RELEASE",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // policy
                    ],
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec!["ax_is_release".into()]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AX_PKG_SWIG",
                M4MacroSignature {
                    arg_types: vec![
                        Lit,  // major.minor.micro
                        Cmds, // action-if-found
                        Cmds, // action-if-not-found
                    ],
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        Var::define_output("SWIG"),
                        Var::define_output("SWIG_LIB"),
                    ]),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AX_VALGRIND_CHECK",
                M4MacroSignature {
                    shell_vars: Some(vec![
                        Var::define_output("VALGRIND_CHECK_RULES"),
                        Var::define_input("VALGRIND_ENABLED"),
                    ]),
                    ret_type: Some(Cmds),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AX_VALGRIND_DFLT",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // rule
                        Lit, // on/off
                    ],
                    ret_type: Some(Cmds),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AX_CXX_COMPILE_STDCXX",
                M4MacroSignature {
                    arg_types: vec![
                        Lit, // version
                        Lit, // ext/noext
                        Lit, // mandatory/optional
                        Lit, // (undocumented)
                    ],
                    shell_vars: Some(vec![Var::reference("CXX"), Var::reference("CXXCPP")]),
                    cpp_symbols: Some(vec![
                        CPP::set("HAVE_CXX23"),
                        CPP::set("HAVE_CXX20"),
                        CPP::set("HAVE_CXX17"),
                        CPP::set("HAVE_CXX14"),
                        CPP::set("HAVE_CXX11"),
                    ]),
                    ret_type: Some(Cmds),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AX_GCC_ARCHFLAG",
                M4MacroSignature {
                    arg_types: vec![
                        Lit,  // portable?
                        Cmds, // action-success
                        Cmds, // action-failure
                    ],
                    shell_vars: Some(vec!["ax_cv_gcc_arcflag".into()]),
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AX_GCC_BUILTIN",
                M4MacroSignature {
                    arg_types: vec![VarName(
                        None,
                        Some(&|s| {
                            // builtin
                            vec![
                                ExVar(
                                    VarAttrs::default(),
                                    format!("ax_cv_have_{}", sanitize_shell_name(s)),
                                    Some("yes".into()),
                                ),
                                ExCPP(
                                    format!("HAVE_{}", sanitize_c_name(s)),
                                    Some(Some("1".into())),
                                ),
                            ]
                        }),
                    )],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AX_GCC_CONST_CALL",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AX_GCC_FUNC_ATTRIBUTE",
                M4MacroSignature {
                    arg_types: vec![VarName(
                        None,
                        Some(&|s| {
                            // attribute
                            vec![
                                ExVar(
                                    VarAttrs::default(),
                                    format!("ax_cv_have_func_attribute_{}", sanitize_shell_name(s)),
                                    Some("yes".into()),
                                ),
                                ExCPP(
                                    format!("HAVE_FUNC_ATTRIBUTE_{}", sanitize_c_name(s)),
                                    Some(Some("1".into())),
                                ),
                            ]
                        }),
                    )],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AX_GCC_LIB",
                M4MacroSignature {
                    arg_types: vec![
                        Library(None), // library
                        Cmds,          // action-if-found
                        Cmds,          // action-if-not-found
                    ],
                    shell_vars: Some(vec!["ax_cv_gcc_arcflag".into()]),
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AX_GCC_LIBGCC_EH",
                M4MacroSignature {
                    arg_types: vec![
                        VarName(Some(VarAttrs::default()), None), // variable
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AX_HAVE_ADNS",
                M4MacroSignature {
                    arg_types: vec![
                        Cmds, // [action-if-found]
                        Cmds, // [action-if-not-found]
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AX_HAVE_POLL",
                M4MacroSignature {
                    arg_types: vec![
                        Cmds, // [action-if-found]
                        Cmds, // [action-if-not-found]
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AX_HAVE_PPOLL",
                M4MacroSignature {
                    arg_types: vec![
                        Cmds, // [action-if-found]
                        Cmds, // [action-if-not-found]
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AX_HAVE_QT",
                M4MacroSignature {
                    shell_vars: Some(vec![
                        "have_qt".into(),
                        Var::define_output("QT_CXXFLAGS"),
                        Var::define_output("QT_LIBS"),
                        Var::define_output("QT_MOC"),
                        Var::define_output("QT_UIC"),
                        Var::define_output("QT_RCC"),
                        Var::define_output("QT_LRELEASE"),
                        Var::define_output("QT_LUPDATE"),
                        Var::define_output("QT_DIR"),
                        Var::define_output("QMAKE"),
                    ]),
                    ret_type: Some(Cmds),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AX_HAVE_SELECT",
                M4MacroSignature {
                    arg_types: vec![
                        Cmds, // [action-if-found]
                        Cmds, // [action-if-not-found]
                    ],
                    ret_type: Some(Cmds),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AX_HAVE_PSELECT",
                M4MacroSignature {
                    arg_types: vec![
                        Cmds, // [action-if-found]
                        Cmds, // [action-if-not-found]
                    ],
                    ret_type: Some(Cmds),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AX_INCLUDE_STRCASECMP",
                M4MacroSignature {
                    cpp_symbols: Some(vec![CPP::with_value("AX_STRCASECMP_HEADER", "<strings.h>")]),
                    ret_type: Some(Cmds),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AX_INSTALL_FILES",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AX_C_LONG_LONG",
                M4MacroSignature {
                    cpp_symbols: Some(vec![CPP::set("HAVE_LONG_LONG")]),
                    ret_type: Some(Cmds),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AX_C_RESTRICT",
                M4MacroSignature {
                    cpp_symbols: Some(vec![CPP::unset("restrict")]),
                    ret_type: Some(Cmds),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AX_PRINTF_SIZE_T",
                M4MacroSignature {
                    cpp_symbols: Some(vec![CPP::with_value("PRI_SIZE_T_MODIFIER", "z")]),
                    ret_type: Some(Cmds),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AX_RECURSIVE_EVAL",
                M4MacroSignature {
                    arg_types: vec![
                        Word,                                     // value
                        VarName(Some(VarAttrs::default()), None), // result
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AX_APPEND_FLAG",
                M4MacroSignature {
                    arg_types: vec![
                        Lit,                                      // flag
                        VarName(Some(VarAttrs::default()), None), // [flag-variable] or LDFLAGS
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AX_CHECK_LINK_FLAG",
                M4MacroSignature {
                    arg_types: vec![
                        Lit,  // flag
                        Cmds, // [action-success]
                        Cmds, // [action-failure]
                        Lit,  // [extra-flags]
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AX_APPEND_LINK_FLAGS",
                M4MacroSignature {
                    arg_types: vec![
                        Lit,                                      // flag
                        VarName(Some(VarAttrs::default()), None), // flag-variable
                    ],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AX_PTHREAD",
                M4MacroSignature {
                    arg_types: vec![
                        Cmds, // [action-if-found]
                        Cmds, // [action-if-not-found]
                    ],
                    shell_vars: Some(vec![
                        Var::reference("GCC"),
                        Var::define_output("PTHREAD_LIBS").with_value("-lpthread"),
                        Var::define_output("PTHREAD_CFLAGS").with_value("-pthread"),
                        Var::define_output("PTHREAD_CC").with_value("cc"),
                        Var::define_output("PTHREAD_CXX").with_value("c++"),
                        Var::define_internal("ax_pthread_ok").yes(),
                        Var::define_internal("ax_pthread_clang").yes(),
                    ]),
                    cpp_symbols: Some(vec![
                        CPP::set("HAVE_PTHREAD"),
                        CPP::set("HAVE_PTHREAD_PRIO_INHERIT"),
                        CPP::unset("PTHREAD_CREATE_JOINABLE"),
                    ]),
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AX_C___ATTRIBUTE__",
                M4MacroSignature {
                    cpp_symbols: Some(vec![CPP::set("HAVE___ATTRIBUTE__")]),
                    ret_type: Some(Cmds),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AX_COMPILER_VENDOR",
                M4MacroSignature {
                    shell_vars: Some(vec![
                        Var::define_internal("ax_cv_c_compiler_vendor").with_value("gnu"),
                        Var::define_internal("ax_cv_cxx_compiler_vendor").with_value("gnu"),
                        Var::define_internal("ax_cv_fc_compiler_vendor").with_value("gnu"),
                    ]),
                    ret_type: Some(Cmds),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AX_CHECK_FRAMEWORK",
                // FIXME: actually this just works when host_os = "xdarwin*" (on mac x).
                // in the future we will take platform branching into account to correctly fix values.
                M4MacroSignature {
                    arg_types: vec![
                        VarName(
                            None,
                            Some(&|s| {
                                // framework
                                vec![ExVar(
                                    VarAttrs::append(Some(Output)),
                                    "LIBS".into(),
                                    Some(format!("-framework {}", s)),
                                )]
                            }),
                        ),
                        Cmds, // [action-if-found]
                        Cmds, // [action-if-not-found]
                    ],
                    shell_vars: Some(vec![]),
                    ret_type: Some(Cmds),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
            (
                "AX_HAVE_OPENCL",
                M4MacroSignature {
                    arg_types: vec![Lit],
                    shell_vars: Some(vec![
                        Var::define_input("enable_opencl"),
                        Var::define_internal("CL_ENABLED"),
                        Var::define_internal("CL_CPPFLAGS"),
                        Var::define_internal("CL_VERSION"),
                        Var::define_internal("CL_LIBS"),
                        Var::define_internal("no_cl"),
                    ]),
                    cpp_symbols: Some(vec!["HAVE_CL_CL_H".into()]),
                    ret_type: Some(Cmds),
                    is_oneshot: true,
                    ..Default::default()
                },
            ),
        ]
        .map(|(name, t)| (name.to_string(), t)),
    )
}
