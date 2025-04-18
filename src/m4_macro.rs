//! Provide data structures related to m4 macros.
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
    Paths(
        ArrayDelim,
        Option<&'static (dyn Fn(&str) -> Vec<(M4ExportType, String)> + Sync)>,
    ),
    /// the type string include struct member strings, e.g. `struct A.member`
    Type(Option<M4ExportFunc>),
    /// array of type strings separated by comma (expecting the array to be enclosed by quotes)
    Types(ArrayDelim, Option<M4ExportFunc>),
    /// output shell variable name, conversions may be applied to create other variable names.
    VarName(Option<VarAttrs>, Option<M4ExportFunc>),
    /// library name , conversions may be applied to become variable names.
    Library(Option<M4ExportFunc>),
    /// C preprocessor symbol name, conversions may be applied to become variable names.
    CPP,
    /// C symbol, conversions may be applied to become variable names.
    Symbol(Option<M4ExportFunc>),
    /// array of C symbols, separated by whitespace.
    Symbols(ArrayDelim, Option<M4ExportFunc>),
    /// Automake conditional name.
    AMCond,
}

/// A lambda function exporging side effects.
pub type M4ExportFunc = &'static (dyn Fn(&str) -> Vec<(M4ExportType, String)> + Sync);

/// The type of any dynamically exported information
#[derive(Debug, Clone, Copy)]
pub enum M4ExportType {
    /// Export a shell variable
    ExVar(VarAttrs),
    /// Export a C preprocessor symbol
    ExCPP,
    /// Reference to a path
    ExPath,
    /// Export to automake via AM_CONDITIONAL
    ExAMCond,
}

impl PartialEq for M4Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Lit, _) | (_, Lit) => true,
            (Word, Word) => true,
            (Args, _) | (_, Args) => true,
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
            CPP => "CPP",
            Symbol(_) => "Symbol",
            Symbols(_, _) => "Symbols",
            AMCond => "AMCond",
        })
    }
}

/// Represents an argument of m4 macro call.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum M4Argument<W, C> {
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
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct M4Macro<W, C> {
    /// m4 macro name
    pub name: String,
    /// m4 macro arguments
    pub args: Vec<M4Argument<W, C>>,
    /// side effects from the call
    pub effects: Option<SideEffect>,
}

impl<W, C> M4Macro<W, C> {
    /// Create a new M4 macro call node.
    pub fn new(name: String, args: Vec<M4Argument<W, C>>) -> Self {
        Self::new_with_side_effect(name, args, None)
    }

    /// Create a new M4 macro call node with information about side effects.
    pub fn new_with_side_effect(
        name: String,
        args: Vec<M4Argument<W, C>>,
        effects: Option<SideEffect>,
    ) -> Self {
        Self {
            name,
            args,
            effects,
        }
    }
}

/// Represent side effects that an operation (e.g. macro call) could produce.
#[derive(Debug, PartialEq, Eq, Clone, Default)]
pub struct SideEffect {
    /// exported shell variables.
    pub shell_vars: Option<Vec<Var>>,
    /// exported shell variables.
    pub cpp_symbols: Option<Vec<String>>,
    /// exported path strings.
    pub paths: Option<Vec<String>>,
    /// exported automake conditional variables.
    pub am_conds: Option<Vec<String>>,
}

impl From<M4MacroSignature> for SideEffect {
    fn from(value: M4MacroSignature) -> Self {
        SideEffect {
            shell_vars: value.shell_vars,
            cpp_symbols: value.cpp_symbols,
            paths: value.paths,
            ..Default::default()
        }
    }
}

impl SideEffect {
    /// A shell variable is defined.
    pub fn add_shell_var(&mut self, val: &str, attrs: &VarAttrs) {
        let var = Var(val.into(), *attrs);
        if let Some(v) = &mut self.shell_vars {
            v.push(var)
        } else {
            self.shell_vars = Some(vec![var])
        }
    }

    /// A cpp symbol is defined.
    pub fn add_cpp_symbol(&mut self, val: &str) {
        if let Some(v) = &mut self.cpp_symbols {
            v.push(val.into())
        } else {
            self.cpp_symbols = Some(vec![val.into()])
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

    /// An automake conditional variable is defined.
    pub fn add_am_cond(&mut self, val: &str) {
        if let Some(v) = &mut self.am_conds {
            v.push(val.into())
        } else {
            self.am_conds = Some(vec![val.into()])
        }
    }

    /// Any type of export is triggered.
    pub fn add_side_effect(&mut self, export_type: &M4ExportType, val: &str) {
        match export_type {
            ExVar(attrs) => self.add_shell_var(val, attrs),
            ExCPP => self.add_cpp_symbol(val),
            ExPath => self.add_path(val),
            ExAMCond => self.add_am_cond(val),
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
    pub cpp_symbols: Option<Vec<String>>,
    /// When the macro is obsolete and completely replaced by another macro.
    /// If this field is some, other fields should be empty or none.
    pub replaced_by: Option<String>,
    /// List of macro names to be called without arguments if not. c.f. AC_REQUIRE
    /// We expect required macros have the same return type, and no arguments.
    pub require: Option<Vec<String>>,
    /// List of paths referenced in the macro
    pub paths: Option<Vec<String>>,
}

impl M4MacroSignature {
    /// Return whether the signature has any explicit side effects.
    pub fn has_no_exports(&self) -> bool {
        self.shell_vars.is_none() && self.cpp_symbols.is_none() && self.paths.is_none()
    }
}

/// Represents a static shell variable
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Var(pub String, pub VarAttrs);

impl Var {
    /// return if it has input attribute
    pub fn is_input(&self) -> bool {
        match self.1.kind {
            VarKind::Input | VarKind::Precious | VarKind::Environment => true,
            _ => false,
        }
    }
    /// return if it has output attribute
    pub fn is_output(&self) -> bool {
        match self.1.kind {
            VarKind::Output | VarKind::Precious | VarKind::Environment => true,
            _ => false,
        }
    }

    /// return if it has environmental attribute
    pub fn is_env(&self) -> bool {
        match self.1.kind {
            VarKind::Environment => true,
            _ => false,
        }
    }

    /// return if it has conditional attribute
    pub fn is_am_cond(&self) -> bool {
        match self.1.kind {
            VarKind::Conditional => true,
            _ => false,
        }
    }

    /// return if it is referenced
    pub fn is_used(&self) -> bool {
        match self.1.usage {
            VarUsage::Referenced | VarUsage::Added => true,
            _ => false,
        }
    }

    /// return if it is defined
    pub fn is_defined(&self) -> bool {
        match self.1.usage {
            VarUsage::Defined | VarUsage::Added => true,
            _ => false,
        }
    }
}

impl Var {
    /// Create a new shell variable with specified attributes
    pub fn new(name: &str, kind: VarKind, usage: VarUsage) -> Self {
        Self(name.into(), VarAttrs::new(kind, usage))
    }

    /// Create a new shell variable reference
    pub fn reference(name: &str) -> Self {
        Self::new(name, VarKind::Internal, VarUsage::Referenced)
    }

    /// Create a new shell variable reference with its kind known
    pub fn reference_some(name: &str, kind: VarKind) -> Self {
        Self::new(name, kind, VarUsage::Referenced)
    }

    /// Create a new output shell variable
    pub fn define_output(name: &str) -> Self {
        Self::new(name, VarKind::Output, VarUsage::Defined)
    }

    /// Create a new input shell variable
    pub fn define_input(name: &str) -> Self {
        Self::new(name, VarKind::Input, VarUsage::Defined)
    }

    /// Create a new input+output shell variable
    pub fn define_precious(name: &str) -> Self {
        Self::new(name, VarKind::Precious, VarUsage::Defined)
    }

    /// Create a new environmental variable
    pub fn define_env(name: &str) -> Self {
        Self::new(name, VarKind::Environment, VarUsage::Defined)
    }

    /// Create a new automake conditional variable
    pub fn define_conditional(name: &str) -> Self {
        Self::new(name, VarKind::Conditional, VarUsage::Defined)
    }

    /// Create a shell variable adding operation
    pub fn add(name: &str, kind: VarKind) -> Self {
        Self::new(name, kind, VarUsage::Added)
    }
}

impl From<&str> for Var {
    fn from(value: &str) -> Self {
        Self::new(value, VarKind::Internal, VarUsage::Defined)
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
    /// exported to a automake conditional variable
    Conditional,
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
    Added,
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

    /// Create a new automake conditional variable attributes
    pub fn coditional() -> Self {
        Self::new(VarKind::Conditional, VarUsage::Defined)
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

fn split_tag(tag: &str) -> Vec<(M4ExportType, String)> {
    let mut split = tag.split(":");
    let output = split.next().unwrap();
    let mut ret = vec![(ExPath, output.to_string())];
    if let Some(0) = split.size_hint().1 {
        // default input
        ret.push((ExPath, format!("{}.in", output)));
    } else {
        // inputs (will be concatenated)
        for input in split {
            ret.push((ExPath, input.to_string()));
        }
    }
    ret
}

/// Return macro signature if a predefined macro found.
pub fn get_macro(name: &str) -> Option<(&String, &M4MacroSignature)> {
    MACROS.get_key_value(name).map(|(key, signature)| {
        if let Some(ref alternative) = signature.replaced_by {
            MACROS.get_key_value(alternative).unwrap()
        } else {
            (key, signature)
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
                        "ac_default_prefix".into(), // /usr/local
                        "ac_clean_CONFIG_STATUS".into(),
                        "ac_clean_files".into(),
                        "ac_config_libobj_dir".into(),
                        "LIBOBJS".into(),
                        "subdirs".into(),
                        "MFLAGS".into(),
                        "MAKEFLAGS".into(),
                        Var::define_output("SHELL"),
                        Var::define_env("PATH_SEPARATOR"),
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
                        Var::define_input("srcdir"),     // --srcdir
                        Var::define_input("verbose"),    // --verbose
                        Var::define_input("x_includes"), // --x-includes
                        Var::define_input("x_libraries"), // --x-libraries
                        Var::define_input("prefix"),     // --prefix
                        Var::define_input("program_prefix"), // --program-prefix
                        Var::define_input("program_suffix"), // --program-suffix
                        Var::define_input("program_transform_name"), // --program-transform-name
                        Var::define_input("program_suffix"), // --program-suffix
                        Var::define_precious("bindir"),  // ${exec_prefix}/bin
                        Var::define_precious("sbindir"), // ${exec_prefix}/sbin
                        Var::define_precious("libexecdir"), // ${exec_prefix}/libexec
                        Var::define_precious("datarootdir"), // ${prefix}/share
                        Var::define_precious("datadir"), // ${datarootdir}
                        Var::define_precious("sysconfdir"), // ${prefix}/etc
                        Var::define_precious("sharedstatedir"), // ${prefix}/com
                        Var::define_precious("localstatedir"), // ${prefix}/var
                        Var::define_precious("runstatedir"), // ${localstatedir}/run
                        Var::define_precious("includedir"), // ${prefix}/include
                        Var::define_precious("oldincludedir"), // /usr/include
                        Var::define_precious("docdir"),  // ${datarootdir}/doc/${PACKAGE}
                        Var::define_precious("infodir"), // ${datarootdir}/info
                        Var::define_precious("htmldir"), // ${docdir}
                        Var::define_precious("dvidir"),  // ${docdir}
                        Var::define_precious("pdfdir"),  // ${docdir}
                        Var::define_precious("psdir"),   // ${docdir}
                        Var::define_precious("libdir"),  // ${exec_prefix}/lib
                        Var::define_precious("localedir"), // ${datarootdir}/locale
                        Var::define_precious("mandir"),  // ${datarootdir}/man
                        Var::define_precious("host"),    // $host
                        Var::define_precious("host_alias"), // $host_alias
                        Var::define_precious("build"),   // raw argument
                        Var::define_precious("build_alias"), // canonicalized
                        Var::define_precious("target"),  // $build_alias
                        Var::define_precious("target_alias"), // $build_alias
                        "cross_compiling".into(),        // $host_alias != $build_alias
                        "ac_tool_prefix".into(),         // "$host_alias-"
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
                        Var::define_output("am__isrc"),  // -I$(srcdir)
                        Var::define_output("CYGPATH_W"), // cygpath -w, or echo
                        Var::define_output("PACKAGE"),
                        Var::define_output("VERSION"),
                        Var::define_output("mkdir_p"),
                        Var::define_output("AMTAR"),
                        Var::define_output("am__tar"),
                        Var::define_output("am__untar"),
                        Var::define_output("CTAGS"),
                        Var::define_output("ETAGS"),
                        Var::define_output("CSCOPE"),
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
                        Var::define_output("INSTALL_STRIP_PROGRAM"),
                        // by AM_SET_LEADING_DOT
                        Var::define_output("am__leading_dot"),
                        // by AM_SET_DEPDIR
                        Var::define_output("DEPDIR"),
                        // by AM_MAKE_INCLUDE
                        Var::define_output("am__include"),
                        Var::define_output("am__quote"),
                        // by AM_DEP_TRACK
                        Var::define_input("enable_dependency_tracking"),
                        Var::define_output("AMDEPBACKSLASH"),
                        Var::define_output("am__nodep"),
                        Var::define_conditional("AMDEP"),
                        // by _AM_DEPENDENCIES
                        Var::define_output("CCDEPMODE"),
                        Var::define_conditional("am__fastdepCC"),
                        Var::define_output("CXXDEPMODE"),
                        Var::define_conditional("am__fastdepCXX"),
                        Var::define_output("OBJCDEPMODE"),
                        Var::define_conditional("am__fastdepOBJC"),
                        Var::define_output("OBJCXXDEPMODE"),
                        Var::define_conditional("am__fastdepOBJCXX"),
                        // if _AM_COMPILER_EXEEXT is provided
                        Var::define_conditional("am__EXEEXT"), // from $EXEEXT
                    ]),
                    cpp_symbols: Some(vec!["PACKAGE".into(), "VERSION".into()]),
                    require: Some(vec![
                        "AC_PROG_INSTALL".into(),
                        "AC_PROG_MKDIR_P".into(),
                        "AC_PROG_AWK".into(),
                        "AC_PROG_MAKE_SET".into(),
                    ]),
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
                    ..Default::default()
                },
            ),
            (
                "AC_AUTOCONF_VERSION",
                M4MacroSignature {
                    ret_type: Some(Lit), // TODO: btw the return type should be Word?
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
                        Var::define_output("SET_MAKE"),
                        Var::define_output("MAKE"),
                    ]),
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
                        Var::define_output("DEFS"),
                    ]),
                    cpp_symbols: Some(vec!["HAVE_CONFIG_H".into()]),
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
                        CPP, // key
                        Lit, // description
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
                                s.split(":").map(|t| (ExPath, t.to_string())).collect()
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
                                (ExPath, format!("{}/configure", s)),
                                (ExPath, format!("{}/configure.gnu", s)),
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
                        "ac_includes_default".into(),
                        "ac_cv_header_stdlib_h".into(),
                        "ac_cv_header_string_h".into(),
                    ]),
                    cpp_symbols: Some(vec!["STDC_HEADERS".into()]),
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
                    ..Default::default()
                },
            ),
            // paticular program checks
            (
                "AC_PROG_AR",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_output("AR"), "ac_cv_prog_ar".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_PROG_AWK",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_output("AWK"), "ac_cv_prog_AWK".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_PROG_GREP",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_output("GREP"), "ac_cv_path_GREP".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_PROG_EGREP",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_output("EGREP"), "ac_cv_path_EGREP".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_PROG_FGREP",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_output("FGREP"), "ac_cv_path_FGREP".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_PROG_INSTALL",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        Var::define_output("INSTALL"),
                        Var::define_output("INSTALL_PROGRAM"),
                        Var::define_output("INSTALL_SCRIPT"),
                        Var::define_output("INSTALL_DATA"),
                        "ac_cv_path_install".into(),
                    ]),
                    require: Some(vec![
                        // _AC_INIT_AUX_DIR is an internal macro required by
                        // AC_REQUIRE_AUX_FILE that is called by AC_PROG_INSTALL.
                        // Instead we require an equivalent macro: AC_CONFIG_AUX_DIR_DEFAULT.
                        "AC_CONFIG_AUX_DIR_DEFAULT".into(),
                    ]),
                    ..Default::default()
                },
            ),
            (
                "AC_PROG_MKDIR_P",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        Var::define_output("MKDIR_P"),
                        "ac_cv_path_mkdir".into(),
                    ]),
                    ..Default::default()
                },
            ),
            (
                "AC_PROG_LEX",
                M4MacroSignature {
                    arg_types: vec![Arr(Blank)],
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        Var::define_output("LEX"),
                        Var::define_output("LEX_OUTPUT_ROOT"),
                        Var::define_output("LEXLIB"),
                        "ac_cv_prog_LEX".into(),
                    ]),
                    cpp_symbols: Some(vec!["YYTEXT_POINTER".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_PROG_LN_S",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_output("LN_S")]),
                    ..Default::default()
                },
            ),
            (
                "AC_PROG_RANLIB",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        Var::define_output("RANLIB"),
                        "ac_cv_prog_ranlib".into(),
                    ]),
                    ..Default::default()
                },
            ),
            (
                "AC_PROG_SED",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_output("SED"), "ac_cv_path_SED".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_PROG_YACC",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_output("YACC"), "ac_cv_prog_YACC".into()]),
                    ..Default::default()
                },
            ),
            // Automake macros
            // the first argument of `AM_CONDITIONAL` denotes a variable
            // to be used in Makefile.am, not in shell
            (
                "AM_CONDITIONAL",
                M4MacroSignature {
                    arg_types: vec![AMCond, Cmds],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AM_COND_IF",
                M4MacroSignature {
                    arg_types: vec![
                        AMCond, // conditional
                        Cmds,   // [if-true]
                        Cmds,   // [if-false]
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
                    shell_vars: Some(vec![Var::define_output("AR")]),
                    paths: Some(vec!["ar-lib".into()]), // prefix is $ac_aux_dir
                    ..Default::default()
                },
            ),
            (
                "AM_PROG_AS",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        Var::define_precious("CCAS"),
                        Var::define_precious("CCASFLAGS"),
                    ]),
                    require: Some(vec!["AC_PROG_CC".into()]),
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
                    ..Default::default()
                },
            ),
            (
                "AM_PROG_MKDIR_P",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_output("mkdir_p")]),
                    require: Some(vec!["AC_PROG_MKDIR_P".into()]),
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
                        Var::define_output("LEX"),
                        Var::define_output("LEX_OUTPUT_ROOT"),
                        Var::define_output("LEXLIB"),
                        "ac_cv_prog_LEX".into(),
                    ]),
                    cpp_symbols: Some(vec!["YYTEXT_POINTER".into()]),
                    paths: Some(vec![
                        // by AC_REQUIRE_AUX_FILE([missing])
                        "missing".into(), // prefix is $ac_aux_dir
                    ]),
                    ..Default::default()
                },
            ),
            (
                "AM_PROG_GCJ",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        // the differece from AC_PROG_LEX is the LEX's value when lex is not found.
                        // missing script will be triggered instead.
                        Var::define_output("GCJ"),
                        Var::define_output("GCJFLAGS"),
                        "ac_cv_prog_GCJ".into(),
                        // by _AM_DEPENDENCIES([GCJ])
                        Var::define_output("GCJDEPMODE"),
                        Var::define_conditional("am__fastdepGCJ"),
                    ]),
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
                        Var::define_conditional("am__fastdepUPC"),
                    ]),
                    require: Some(vec!["AC_PROG_CC".into()]),
                    ..Default::default()
                },
            ),
            (
                "AM_MISSING_PROG",
                M4MacroSignature {
                    arg_types: vec![
                        VarName(Some(VarAttrs::output()), None), // name
                        Word,                                    // program
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
                        Var::define_output("AM_V"),
                        Var::define_output("AM_DEFAULT_V"),
                        Var::define_output("AM_DEFAULT_VERBOSITY"),
                        Var::define_output("AM_BACKSLASH"),
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
                        Var::define_input("with_dmalloc"),
                        // '-ldmalloc' is added to LIBS
                        Var::add("LIBS", Output),
                        // '-g' is added to LDFLAGS
                        Var::add("LDFLAGS", Output),
                    ]),
                    cpp_symbols: Some(vec!["WITH_DMALLOC".into()]),
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
                                vec![(ExVar(VarAttrs::internal()), format!("ac_cv_prog_{}", s))]
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
                                vec![(ExVar(VarAttrs::internal()), format!("ac_cv_prog_{}", s))]
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
                                vec![(ExVar(VarAttrs::internal()), format!("ac_cv_prog_{}", s))]
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
                                vec![(ExVar(VarAttrs::internal()), format!("ac_cv_prog_{}", s))]
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
                                vec![(ExVar(VarAttrs::internal()), format!("ac_cv_prog_{}", s))]
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
                                vec![(ExVar(VarAttrs::internal()), format!("ac_cv_prog_{}", s))]
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
                                vec![(ExVar(VarAttrs::internal()), format!("ac_cv_path_{}", s))]
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
                                vec![(ExVar(VarAttrs::internal()), format!("ac_cv_path_{}", s))]
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
                                vec![(ExVar(VarAttrs::internal()), format!("ac_cv_path_{}", s))]
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
                                vec![(ExVar(VarAttrs::internal()), format!("ac_cv_prog_{}", s))]
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
                                vec![(ExVar(VarAttrs::internal()), format!("ac_cv_prog_{}", s))]
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
                            vec![(
                                ExVar(VarAttrs::new(Internal, Defined)),
                                format!("ac_cv_file_{}", sanitize_shell_name(s)),
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
                                    (
                                        ExVar(VarAttrs::internal()),
                                        format!("ac_cv_file_{}", sanitize_shell_name(s)),
                                    ),
                                    (ExCPP, format!("HAVE_{}", sanitize_c_name(s))),
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
                                // but to define it needs the two values which is not supported here.
                                (ExCPP, format!("HAVE_LIB{}", sanitize_c_name(s))),
                            ]
                        })),
                        Symbol(None), // function
                        Cmds,         // [action-if-found]
                        Cmds,         // [action-if-not-found]
                        Arr(Blank),   // [other-libraries]
                    ],
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        // '-l<library>' is added
                        Var::add("LIBS", Output),
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
                            vec![(ExCPP, format!("HAVE_LIB{}", sanitize_c_name(s)))]
                        })),
                        Cmds,       // [action-if-found]
                        Cmds,       // [action-if-not-found]
                        Arr(Blank), // [other-libraries]
                    ],
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        // '-l<library>' is added
                        Var::add("LIBS", Output),
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
                            vec![(ExVar(VarAttrs::internal()), format!("ac_cv_search_{}", s))]
                        })),
                        Arr(Blank), // search-libs
                        Cmds,       // [action-if-found]
                        Cmds,       // [action-if-not-found]
                        Arr(Blank), // [other-libraries]
                    ],
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        // '-l<library>' is added
                        Var::add("LIBS", Output),
                    ]),
                    ..Default::default()
                },
            ),
            // Paticular function checks
            (
                "AC_FUNC_ALLOCA",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_output("ALLOCA")]),
                    cpp_symbols: Some(vec![
                        "HAVE_ALLOC_H".into(),
                        "HAVE_ALLOCA".into(),
                        "C_ALLOCA".into(),
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
                    shell_vars: Some(vec!["ac_cv_func_chown_works".into()]),
                    cpp_symbols: Some(vec!["HAVE_CHOWN".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_FUNC_CLOSEDIR_VOID",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec!["ac_cv_func_closedir_void".into()]),
                    cpp_symbols: Some(vec!["CLOSEDIR_VOID".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_DIR_HEADER", // obsolete.
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        // AC_HEADER_DIRENT
                        "ac_header_dirent".into(),
                        "ac_cv_search_opendir".into(),
                        // AC_FUNC_CLOSEDIR_VOID
                        "ac_cv_func_closedir_void".into(),
                    ]),
                    cpp_symbols: Some(vec![
                        // AC_HEADER_DIRENT
                        "HAVE_DIRENT_H".into(),
                        "HAVE_SYS_NDIR_H".into(),
                        "HAVE_SYS_DIR_H".into(),
                        "HAVE_NDIR_H".into(),
                        // AC_FUNC_CLOSEDIR_VOID
                        "CLOSEDIR_VOID".into(),
                        // old symbols
                        "DIRENT".into(),
                        "SYSNDIR".into(),
                        "SYSDIR".into(),
                        "NDIR".into(),
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
                    shell_vars: Some(vec!["ac_cv_lib_error_at_line".into()]),
                    paths: Some(vec!["path.c".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_FUNC_FNMATCH",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec!["ac_cv_func_fnmatch_works".into()]),
                    cpp_symbols: Some(vec!["HAVE_FNMATCH".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_FUNC_FNMATCH_GNU",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec!["ac_cv_func_fnmatch_gnu".into()]),
                    cpp_symbols: Some(vec!["HAVE_FNMATCH".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_FUNC_FORK",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        "ac_cv_func_fork_works".into(),
                        "ac_cv_func_vfork_works".into(),
                        "ac_cv_func_fork".into(),
                        "ac_cv_func_vfork".into(),
                    ]),
                    cpp_symbols: Some(vec!["HAVE_WORKING_FORK".into(), "HAVE_VFORK".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_FUNC_FSEEKO",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    cpp_symbols: Some(vec!["HAVE_FSEEKO".into(), "_LARGEFILE_SOURCE".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_FUNC_GETGROUPS",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_output("GETGROUPS_LIB")]),
                    cpp_symbols: Some(vec!["HAVE_GETGROUPS".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_FUNC_GETLOADAVG",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        Var::define_output("GETLOADAVG_LIBS"),
                        // '-l<lib>' for lib in GETLOADAVG_LIBS
                        Var::add("LIBS", Output),
                        // if the system does not have the 'getloadavg' function.
                        Var::define_output("NEED_SETGID"),
                        Var::define_output("KMEM_GROUP"),
                    ]),
                    cpp_symbols: Some(vec![
                        "HAVE_GETLOADAVG".into(),
                        // if the system does not have the 'getloadavg' function.
                        "C_GETLOADAVG".into(),
                        "SVR4".into(),
                        "DGUX".into(),
                        "UMAX".into(),
                        "UMAX4_3".into(),
                        "HAVE_NLIST_H".into(),
                        "HAVE_STRUCT_NLIST_N_UN_N_NAME".into(),
                        "GETLOADAVG_PREVILEGED".into(),
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
                        "ac_cv_func_getmntent".into(),
                        "ac_cv_search_getmntent".into(),
                    ]),
                    cpp_symbols: Some(vec!["HAVE_SETMNTENT".into()]),
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
                "AC_EXEEXT", // obsolete. now done automaticallly.
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_output("EXEEXT")]),
                    ..Default::default()
                },
            ),
            (
                "AC_FUNC_GETPGRP",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec!["ac_cv_func_getpgrp_void".into()]),
                    cpp_symbols: Some(vec!["GETPGRP_VOID".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_FUNC_LSTAT_FOLLOWS_SLASHED_SYMLINK",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec!["ac_cv_func_lstat_dereferences_slashed_symlink".into()]),
                    cpp_symbols: Some(vec!["LSTAT_FOLLOWS_SYMLINK".into()]),
                    paths: Some(vec!["lstat.c".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_FUNC_MALLOC",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec!["ac_cv_func_func_malloc_0_nonnull".into()]),
                    cpp_symbols: Some(vec![
                        "HAVE_MALLOC".into(),
                        // if the 'macro' function is not compatible to GNU C
                        "malloc".into(), // overridden by 'rpl_malloc'
                    ]),
                    paths: Some(vec!["malloc.c".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_FUNC_MBRTOWC",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec!["ac_cv_func_mbrtowc".into()]),
                    cpp_symbols: Some(vec!["HAVE_MBRTOWC".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_FUNC_MEMCMP",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec!["ac_cv_func_memcmp_working".into()]),
                    paths: Some(vec!["memcmp.c".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_FUNC_MKTIME",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec!["ac_cv_func_working_mktime".into()]),
                    paths: Some(vec!["mktime.c".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_FUNC_MMAP",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec!["ac_cv_func_mmap_fixed_mapped".into()]),
                    cpp_symbols: Some(vec!["HAVE_MMAP".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_FUNC_OBSTACK",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec!["ac_cv_func_obstack".into()]),
                    cpp_symbols: Some(vec!["HAVE_OBSTACK".into()]),
                    paths: Some(vec!["obstack.c".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_FUNC_REALLOC",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec!["ac_cv_func_realloc_0_nonnull".into()]),
                    cpp_symbols: Some(vec!["HAVE_REALLOC".into(), "realloc".into()]),
                    paths: Some(vec!["realloc.c".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_FUNC_SELECT_ARGTYPES",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    cpp_symbols: Some(vec![
                        "SELECT_TYPE_ARG1".into(),
                        "SELECT_TYPE_ARG234".into(),
                        "SELECT_TYPE_ARG5".into(),
                    ]),
                    ..Default::default()
                },
            ),
            (
                "AC_FUNC_SETPGRP",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec!["ac_cv_func_setpgrp_void".into()]),
                    cpp_symbols: Some(vec!["SETPGRP_VOID".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_FUNC_STAT",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec!["ac_cv_func_stat_empty_string_bug".into()]),
                    cpp_symbols: Some(vec!["HAVE_STAT_EMPTY_STRING_BUG".into()]),
                    paths: Some(vec!["stat.c".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_FUNC_LSTAT",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec!["ac_cv_func_lstat_empty_string_bug".into()]),
                    cpp_symbols: Some(vec!["HAVE_LSTAT_EMPTY_STRING_BUG".into()]),
                    paths: Some(vec!["lstat.c".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_FUNC_STRCOLL",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec!["ac_cv_func_lstat_empty_string_bug".into()]),
                    cpp_symbols: Some(vec!["HAVE_LSTAT_EMPTY_STRING_BUG".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_FUNC_STRERROR_R",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec!["ac_cv_func_stderror_r_char_p".into()]),
                    cpp_symbols: Some(vec!["HAVE_DECL_STDERROR_R".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_FUNC_STRFTIME",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    cpp_symbols: Some(vec!["HAVE_STRFTIME".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_FUNC_STRTOD",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        Var::define_output("POW_LIB"),
                        "ac_cv_func_strtod".into(),
                        "ac_cv_func_pow".into(),
                    ]),
                    paths: Some(vec!["strtod.c".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_FUNC_STRTOLD",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec!["ac_cv_func_strtold".into()]),
                    cpp_symbols: Some(vec!["HAVE_STRTOLD".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_FUNC_STRNLEN",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec!["ac_cv_func_strnlen_working".into()]),
                    cpp_symbols: Some(vec!["HAVE_STRTOLD".into()]),
                    paths: Some(vec!["strnlen.c".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_FUNC_UTIME_NULL",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec!["ac_cv_func_utime_null".into()]),
                    cpp_symbols: Some(vec!["HAVE_UTIME_NULL".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_FUNC_VPRINTF",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    cpp_symbols: Some(vec![
                        "HAVE_VPRINTF".into(),
                        // otherwise if '_doprnt' if found
                        "HAVE_DOPRNT".into(),
                    ]),
                    ..Default::default()
                },
            ),
            (
                "AC_REPLACE_FNMATCH",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec!["ac_cv_func_fnmatch_works".into()]),
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
                            vec![(ExVar(VarAttrs::internal()), format!("ac_cv_func_{}", s))]
                        })),
                        Cmds, // [action-if-found]
                        Cmds, // [action-if-not-found]
                    ],
                    ret_type: Some(Cmds),
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
                                    (ExVar(VarAttrs::internal()), format!("ac_cv_func_{}", s)),
                                    (ExCPP, format!("HAVE_{}", sanitize_c_name(s))),
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
                                (ExVar(VarAttrs::internal()), format!("ac_cv_func_{}", s)),
                                (ExCPP, format!("HAVE_{}", sanitize_c_name(s))),
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
                        vec![(ExPath, format!("{}.c", s))]
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
                                (ExVar(VarAttrs::internal()), format!("ac_cv_func_{}", s)),
                                (ExCPP, format!("HAVE_{}", sanitize_c_name(s))),
                                (ExPath, format!("{}.c", s)),
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
                    shell_vars: Some(vec!["ac_cv_header_stdbool_h".into()]),
                    cpp_symbols: Some(vec!["HAVE__BOOL".into()]),
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
                        Var::define_input("ac_enable_assert"),
                    ]),
                    cpp_symbols: Some(vec!["NDEBUG".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_HEADER_DIRENT",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        "ac_header_dirent".into(),
                        "ac_cv_search_opendir".into(),
                    ]),
                    cpp_symbols: Some(vec![
                        "HAVE_DIRENT_H".into(),
                        "HAVE_SYS_NDIR_H".into(),
                        "HAVE_SYS_DIR_H".into(),
                        "HAVE_NDIR_H".into(),
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
                    shell_vars: Some(vec!["ac_cv_header_sys_mkdev_h".into()]),
                    cpp_symbols: Some(vec!["MAJOR_IN_MKDEV".into(), "MAJOR_IN_SYSMACROS".into()]),
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
                    shell_vars: Some(vec!["ac_cv_header_sys_mkdev_h".into()]),
                    cpp_symbols: Some(vec!["MAJOR_IN_MKDEV".into(), "MAJOR_IN_SYSMACROS".into()]),
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
                    shell_vars: Some(vec!["ac_cv_header_stat_broken".into()]),
                    cpp_symbols: Some(vec!["STAT_MACROS_BROKEN".into()]),
                    paths: Some(vec!["sys/stat.h".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_HEADER_STDBOOL",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec!["ac_cv_header_stdbool_h".into()]),
                    cpp_symbols: Some(vec!["HAVE_STDBOOL_H".into(), "HAVE__BOOL".into()]),
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
                    shell_vars: Some(vec!["ac_cv_header_sys_wait_h".into()]),
                    cpp_symbols: Some(vec![
                        "HAVE_SYS_WAIT_H".into(),
                        // if 'unistd.h' is included on Posix systems
                        "_POSIX_VERSION".into(),
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
                        "ac_cv_sys_tiocgwinsz_in_termios_h".into(),
                        "ac_cv_sys_tiocgwinsz_in_sys_ioctl_h".into(),
                    ]),
                    cpp_symbols: Some(vec!["GWINSZ_IN_SYS_IOCTL".into()]),
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
                            vec![(
                                ExVar(VarAttrs::internal()),
                                format!("ac_cv_header_{}", sanitize_shell_name(s)),
                            )]
                        })),
                        Cmds, // [action-if-found]
                        Cmds, // [action-if-not-found]
                        Prog, // [includes]
                    ],
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
                                    (
                                        ExVar(VarAttrs::internal()),
                                        format!("ac_cv_header_{}", sanitize_shell_name(s)),
                                    ),
                                    (ExCPP, format!("HAVE_{}", sanitize_c_name(s))),
                                ]
                            }),
                        ),
                        Cmds, // [action-if-found]
                        Cmds, // [action-if-not-found]
                        Prog, // [includes]
                    ],
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
                                (
                                    ExVar(VarAttrs::internal()),
                                    format!("ac_cv_header_{}", sanitize_shell_name(s)),
                                ),
                                (ExCPP, format!("HAVE_{}", sanitize_c_name(s))),
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
                            vec![(
                                ExVar(VarAttrs::internal()),
                                format!("ac_cv_have_decl_{}", sanitize_shell_name(s)),
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
                                    (
                                        ExVar(VarAttrs::internal()),
                                        format!("ac_cv_have_decl_{}", sanitize_shell_name(s)),
                                    ),
                                    (ExCPP, format!("HAVE_DECL_{}", sanitize_shell_name(s))),
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
                                (
                                    ExVar(VarAttrs::internal()),
                                    format!("ac_cv_have_decl_{}", sanitize_shell_name(s)),
                                ),
                                (ExCPP, format!("HAVE_DECL_{}", sanitize_shell_name(s))),
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
                        "HAVE_STRUCT_DIRENT_D_INO".into(),
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
                        "HAVE_STRUCT_DIRENT_D_TYPE".into(),
                    ]),
                    require: Some(vec!["AC_HEADER_DIRENT".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_STRUCT_ST_BLOCKS",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec!["ac_cv_member_struct_stat_st_blocks".into()]),
                    cpp_symbols: Some(vec![
                        "HAVE_STRUT_ST_BLOCKS".into(),
                        "HAVE_ST_BLOCKS".into(), // deprecated
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
                    cpp_symbols: Some(vec!["TM_IN_SYS_TIME".into()]),
                    paths: Some(vec!["sys/time.h".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_STRUCT_TIMEZONE",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    cpp_symbols: Some(vec![
                        "HAVE_STRUCT_TM_TM_ZONE".into(),
                        "HAVE_TM_ZONE".into(), // deprecated
                        // `struct tm.tm_zone` is not found
                        // and if the external array 'tzname' is found
                        "HAVE_TZNAME".into(),      // if defined
                        "HAVE_DECL_TZNAME".into(), // if declared
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
                            vec![(
                                ExVar(VarAttrs::internal()),
                                format!("ac_cv_member_{}", sanitize_shell_name(s)),
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
                                    (
                                        ExVar(VarAttrs::internal()),
                                        format!("ac_cv_member_{}", sanitize_shell_name(s)),
                                    ),
                                    (ExCPP, format!("HAVE_{}", sanitize_c_name(s))),
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
                    shell_vars: Some(vec!["ac_cv_type_getgroups".into()]),
                    cpp_symbols: Some(vec!["GETGROUPS_T".into()]),
                    paths: Some(vec!["unistd.h".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_TYPE_INT8_T",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec!["ac_cv_c_int8_t".into()]),
                    cpp_symbols: Some(vec!["int8_t".into()]),
                    paths: Some(vec!["stdint.h".into(), "inttypes.h".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_TYPE_INT16_T",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec!["ac_cv_c_int16_t".into()]),
                    cpp_symbols: Some(vec!["int16_t".into()]),
                    paths: Some(vec!["stdint.h".into(), "inttypes.h".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_TYPE_INT32_T",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec!["ac_cv_c_int32_t".into()]),
                    cpp_symbols: Some(vec!["int32_t".into()]),
                    paths: Some(vec!["stdint.h".into(), "inttypes.h".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_TYPE_INT64_T",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec!["ac_cv_c_int64_t".into()]),
                    cpp_symbols: Some(vec!["int64_t".into()]),
                    paths: Some(vec!["stdint.h".into(), "inttypes.h".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_TYPE_INTMAX_T",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec!["ac_cv_type_intmax_t".into()]),
                    cpp_symbols: Some(vec!["HAVE_INTMAX_T".into(), "intmax_t".into()]),
                    paths: Some(vec!["stdint.h".into(), "inttypes.h".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_TYPE_INTPTR_T",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec!["ac_cv_type_intptr_t".into()]),
                    cpp_symbols: Some(vec!["HAVE_INTPTR_T".into(), "intptr_t".into()]),
                    paths: Some(vec!["stdint.h".into(), "inttypes.h".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_TYPE_LONG_DOUBLE",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec!["ac_cv_type_long_double".into()]),
                    cpp_symbols: Some(vec!["HAVE_LONG_DOUBLE".into()]),
                    paths: Some(vec!["stdint.h".into(), "inttypes.h".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_TYPE_LONG_DOUBLE_WIDER",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec!["ac_cv_type_long_double_wider".into()]),
                    cpp_symbols: Some(vec!["HAVE_LONG_DOUBLE_WIDER".into()]),
                    paths: Some(vec!["stdint.h".into(), "inttypes.h".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_TYPE_LONG_LONG_INT",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec!["ac_cv_type_long_long_int".into()]),
                    cpp_symbols: Some(vec!["HAVE_LONG_LONG_INT".into()]),
                    paths: Some(vec!["stdint.h".into(), "inttypes.h".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_TYPE_MBSTATE_T",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec!["ac_cv_type_mbstate_t".into()]),
                    cpp_symbols: Some(vec!["HAVE_MBSTATE_T".into()]),
                    paths: Some(vec!["wchar.h".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_TYPE_MODE_T",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec!["ac_cv_type_mode_t".into()]),
                    cpp_symbols: Some(vec!["mode_t".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_TYPE_OFF_T",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec!["ac_cv_type_off_t".into()]),
                    cpp_symbols: Some(vec!["off_t".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_TYPE_PID_T",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec!["ac_cv_type_pid_t".into()]),
                    cpp_symbols: Some(vec!["pid_t".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_TYPE_SIZE_T",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec!["ac_cv_type_size_t".into()]),
                    cpp_symbols: Some(vec!["size_t".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_TYPE_SSIZE_T",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec!["ac_cv_type_ssize_t".into()]),
                    cpp_symbols: Some(vec!["ssize_t".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_TYPE_UID_T",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec!["ac_cv_type_uid_t".into()]),
                    cpp_symbols: Some(vec!["uid_t".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_TYPE_UINT8_T",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec!["ac_cv_c_uint8_t".into()]),
                    cpp_symbols: Some(vec!["uint8_t".into()]),
                    paths: Some(vec!["stdint.h".into(), "inttypes.h".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_TYPE_UINT16_T",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec!["ac_cv_c_uint16_t".into()]),
                    cpp_symbols: Some(vec!["uint16_t".into()]),
                    paths: Some(vec!["stdint.h".into(), "inttypes.h".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_TYPE_UINT32_T",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec!["ac_cv_c_uint32_t".into()]),
                    cpp_symbols: Some(vec!["uint32_t".into()]),
                    paths: Some(vec!["stdint.h".into(), "inttypes.h".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_TYPE_UINT64_T",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec!["ac_cv_c_uint64_t".into()]),
                    cpp_symbols: Some(vec!["uint64_t".into()]),
                    paths: Some(vec!["stdint.h".into(), "inttypes.h".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_TYPE_UINTMAX_T",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec!["ac_cv_type_uintmax_t".into()]),
                    cpp_symbols: Some(vec!["HAVE_UINTMAX_T".into(), "uintmax_t".into()]),
                    paths: Some(vec!["stdint.h".into(), "inttypes.h".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_TYPE_UINTPTR_T",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec!["ac_cv_type_uintptr_t".into()]),
                    cpp_symbols: Some(vec!["HAVE_UINTPTR_T".into(), "uintptr_t".into()]),
                    paths: Some(vec!["stdint.h".into(), "inttypes.h".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_TYPE_UNSIGNED_LONG_LONG_T",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec!["ac_cv_type_unsigned_long_long_int".into()]),
                    cpp_symbols: Some(vec!["HAVE_UNSIGNED_LONG_LONG_INT".into()]),
                    paths: Some(vec!["stdint.h".into(), "inttypes.h".into()]),
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
                            vec![(
                                ExVar(VarAttrs::internal()),
                                format!(
                                    "ac_cv_type_{}",
                                    sanitize_shell_name(s.replace("*", "p").as_ref())
                                ),
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
                                    (
                                        ExVar(VarAttrs::internal()),
                                        format!(
                                            "ac_cv_type_{}",
                                            sanitize_shell_name(s.replace("*", "p").as_ref())
                                        ),
                                    ),
                                    (ExCPP, format!("HAVE_{}", sanitize_c_name(s))),
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
                                (
                                    ExVar(VarAttrs::internal()),
                                    format!(
                                        "ac_cv_sizeof_{}",
                                        sanitize_shell_name(s.replace("*", "p").as_ref())
                                    ),
                                ),
                                (ExCPP, format!("SIZE_OF_{}", sanitize_c_name(s))),
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
                                (
                                    ExVar(VarAttrs::internal()),
                                    format!(
                                        "ac_cv_align_of_{}",
                                        sanitize_shell_name(s.replace("*", "p").as_ref())
                                    ),
                                ),
                                (ExCPP, format!("ALIGN_OF_{}", sanitize_c_name(s))),
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
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        Var::define_output("OPENMP_CFLAGS"),
                        Var::define_output("OPENMP_CXXFLAGS"),
                        Var::define_output("OPENMP_FFLAGS"),
                        Var::define_output("OPENMP_FCFLAGS"),
                        Var::define_output("OPENMP_FFLAGS"),
                    ]),
                    cpp_symbols: Some(vec!["_OPENMP".into()]),
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
                        Var::define_env("CC"),
                        Var::define_precious("CFLAGS"), // -g -O2
                        Var::define_precious("LDFLAGS"),
                        Var::define_precious("LIBS"),
                        Var::define_precious("OBJC"),
                        Var::define_output("ac_prog_cc_stdc"), // c11/c99/c89/no
                        "GCC".into(), // set to 'yes' if the selected compiler is GNU C
                        Var::reference("ac_tool_prefix"),
                    ]),
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
                    cpp_symbols: Some(vec!["NO_MINUS_C_MINUS_O".into()]),
                    require: Some(vec!["AC_PROG_CC".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_PROG_CPP",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        Var::define_env("CPP"),
                        Var::define_precious("CPPFLAGS"),
                        Var::reference("CC"),
                        "ac_cv_prog_CPP".into(),
                    ]),
                    cpp_symbols: Some(vec!["NO_MINUS_C_MINUS_O".into()]),
                    require: Some(vec!["AC_PROG_CC".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_PROG_CPP_WERROR",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        // turning it on will make CPP treats warnings as errors
                        "ac_c_preproc_warn_flag".into(),
                    ]),
                    require: Some(vec!["AC_PROG_CPP".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_C_BACKSLASH_A",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    cpp_symbols: Some(vec!["HAVE_C_BACKSLASH_A".into()]),
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
                    cpp_symbols: Some(vec![
                        // Define to empty if 'const' does not conform to ANSI C.
                        "const".into(),
                        "ac_cv_c_const".into(),
                    ]),
                    ..Default::default()
                },
            ),
            (
                "AC_C__GENERIC",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    cpp_symbols: Some(vec![
                        // Define to 1 if C11-style _Generic works.
                        "const".into(),
                        "ac_cv_c__Generic".into(),
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
                        "restrict".into(),
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
                        "volatile".into(),
                    ]),
                    ..Default::default()
                },
            ),
            (
                "AC_C_INLINE",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    cpp_symbols: Some(vec![
                        // Define to '__inline__', '__inline', or empty.
                        "inline".into(),
                        "ac_cv_c_inline".into(),
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
                        "HAVE_STRINGSIZE".into(),
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
                        "FLEXIBLE_ARRAY_MEMBER".into(),
                    ]),
                    ..Default::default()
                },
            ),
            (
                "AC_C_VARARRAYS",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    cpp_symbols: Some(vec![
                        "HAVE_C_VARARRAYS".into(), // if supported
                        "__STDC_NO_VLA__".into(),  // if not supported
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
                        "HAVE_TYPEOF".into(),
                        // Define to __typeof__ if the compiler spells it that way.
                        "typeof".into(),
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
                        "PROTOTYPES".into(),
                        // Define like PROTOTYPES; this can be used by system headers.
                        "__PROTOTYPES".into(),
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
                        Var::define_env("CXX"),
                        Var::define_env("CCC"),
                        Var::define_precious("CXXFLAGS"),
                        Var::define_precious("LDFLAGS"),
                        Var::define_precious("LIBS"),
                        "GXX".into(),
                        // cxx11/cxx98/no
                        Var::define_output("ac_prog_cxx_stdcxx"),
                    ]),
                    ..Default::default()
                },
            ),
            (
                "AC_PROG_CXXCPP",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        Var::define_precious("CXXPP"),
                        Var::define_precious("CXXFLAGS"),
                        Var::reference("CXX"),
                        "ac_cv_prog_CXXPP".into(),
                    ]),
                    require: Some(vec!["AC_PROG_CXX".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_PROG_CXX_C_O",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    cpp_symbols: Some(vec!["CXX_NO_MINUS_C_MINUS_O".into()]),
                    require: Some(vec!["AC_PROG_CXX".into()]),
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
                    ..Default::default()
                },
            ),
            (
                "AC_PROG_OBJCPP",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            // Objective C compiler characteristics
            (
                "AC_PROG_OBJCXX",
                M4MacroSignature {
                    arg_types: vec![Arr(Blank)],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_PROG_OBJCXXCPP",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            // Erlang compiler and interpreter characteristics
            (
                "AC_ERLANG_PATH_ERLC",
                M4MacroSignature {
                    arg_types: vec![Lit, Lit],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_ERLANG_NEED_ERLC",
                M4MacroSignature {
                    arg_types: vec![Lit],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_ERLANG_PATH_ERL",
                M4MacroSignature {
                    arg_types: vec![Lit, Lit],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_ERLANG_NEED_ERL",
                M4MacroSignature {
                    arg_types: vec![Lit],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            // Fortran compiler characteristics
            (
                "AC_PROG_F77",
                M4MacroSignature {
                    arg_types: vec![Arr(Blank)],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_PROG_FC",
                M4MacroSignature {
                    arg_types: vec![Arr(Blank), Lit],
                    ret_type: Some(Cmds),
                    ..Default::default()
                },
            ),
            (
                "AC_PROG_F77_C_O",
                M4MacroSignature {
                    ret_type: Some(Cmds),
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
                    ..Default::default()
                },
            ),
            (
                "AC_SYS_INTERPRETER",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        // Define to 'yes' if system supports `#!` in the script.
                        "interpval".into(),
                    ]),
                    ..Default::default()
                },
            ),
            (
                "AC_SYS_LARGEFILE",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        Var::define_input("enable_largefile"),
                        Var::define_input("enable_year2038"),
                        // set to 'yes' if a wide `off_t` is available.
                        "ac_have_largefile".into(),
                        // set to 'yes' if a wide `time_t` is available.
                        "ac_have_year2038".into(),
                    ]),
                    cpp_symbols: Some(vec![
                        // Define to 64 on hosts where this is settable
                        "_FILE_OFFSET_BITS".into(),
                        // Define to 1 on platforms where this makes off_t a 64-bit type
                        "_LARGE_FILES".into(),
                        // by _AC_SYS_YEAR2038_PROBE
                        // Define to 64 on on hosts where this is settable.
                        "_TIME_BITS".into(),
                        // Define to 1 on platforms where this makes time_t a 64-bit type.
                        "__MINGW_USE_VC2005_COMPAT".into(),
                    ]),
                    ..Default::default()
                },
            ),
            (
                "AC_SYS_LONG_FILE_NAMES",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    cpp_symbols: Some(vec![
                        // Define if the system supports file names longer than 14 chars.
                        "HAVE_LONG_FILE_NAMES".into(),
                    ]),
                    ..Default::default()
                },
            ),
            (
                "AC_SYS_POSIX_TERMIOS",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![
                        // Set to 'yes' if 'termios.h' and 'tcgetattr' are supported.
                        "ac_cv_sys_posix_termios".into(),
                    ]),
                    ..Default::default()
                },
            ),
            (
                "AC_SYS_YEAR2038",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    require: Some(vec!["AC_SYS_LARGEFILE".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_SYS_YEAR2038_RECOMMENDED",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    require: Some(vec!["AC_SYS_LARGEFILE".into()]),
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
                        "_ALL_SOURCE".into(),
                        "_DARWIN_C_SOURCCE".into(),
                        "_GNU_SOURCER".into(),
                        "_NETBSD_SOURCE".into(),
                        "_OPENBSD_SOURCE".into(),
                        "_POSIX_PTHREAD_SEMANTICS".into(),
                        "__STDC_WANT_IEC_60559_ATTRIBS_EXT__".into(),
                        "__STDC_WANT_IEC_60559_BEP_EXT__".into(),
                        "__STDC_WANT_IEC_60559_DEP_EXT__".into(),
                        "__STDC_WANT_IEC_60559_EXT__".into(),
                        "__STDC_WANT_IEC_60559_FUNCS_EXT__".into(),
                        "__STDC_WANT_IEC_60559_TYPES_EXT__".into(),
                        "__STDC_WANT_LIB_EXT2__".into(),
                        "__STDC_WANT_MATH_SPEC_FUNCS__".into(),
                        "__TANDEM_SOURCE".into(),
                        // Defined occasionally
                        "__EXTENSIONS__".into(),
                        "__MINIX".into(),
                        "__POSIX_SOURCE".into(),
                        "__POISIX_1_SOURCE".into(),
                        // Define to 500 only if needed to make 'wchar.h' declare 'mbstate_t'.
                        // This is known to be needed on some versions of HP/UX.
                        "__XOPEN_SOURCE".into(),
                        // by _AC_CHECK_HEADER_ONCE
                        "HAVE_WCHAR_H".into(),
                        "HAVE_MINIX_CONFIG_H".into(),
                    ]),
                    paths: Some(vec!["wchar.h".into(), "minix/config.h".into()]),
                    require: Some(vec!["AC_CHECK_INCLUDES_DEFAULT".into()]),
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
                        CPP,  // Note that this argument could not contain any shell vars.
                        Word, // If it contains whiespace, things would be a tragedy.
                        Lit,  // description
                    ],
                    ret_type: Some(CPP),
                    ..Default::default()
                },
            ),
            (
                "AC_DEFINE_UNQUOTED",
                M4MacroSignature {
                    arg_types: vec![
                        CPP,  // if not contains shell vars, this is just a Symbol.
                        Word, // this also can be a shell var
                        Lit,  // description
                    ],
                    ret_type: Some(CPP),
                    ..Default::default()
                },
            ),
            // TODO: restart from here
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
                "AC_SUBST_FILE",
                M4MacroSignature {
                    arg_types: vec![
                        // the contents of the file named by $VARIABLE
                        // will be substituted to @VARIABLE@
                        Path(Some(&|s| {
                            // variable
                            vec![(ExVar(VarAttrs::new(Output, Referenced)), s.into())]
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
                "m4_foreach_w",
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
                    ret_type: Some(CPP),
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
                    ..Default::default()
                },
            ),
            (
                "AS_INIT",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec![Var::define_env("SHELL"), Var::define_env("LC_ALL")]),
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
                    ..Default::default()
                },
            ),
            (
                "AS_LINENO_PREPARE",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec!["LINENO".into()]),
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
                    ..Default::default()
                },
            ),
            (
                "AS_ME_PREPARE",
                M4MacroSignature {
                    ret_type: Some(Cmds),
                    shell_vars: Some(vec!["as_me".into()]),
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
                    ..Default::default()
                },
            ),
            // Site configuration
            (
                "AC_PRESERVE_HELP_ORDER",
                M4MacroSignature {
                    ret_type: Some(Cmds),
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
                                    (
                                        ExVar(VarAttrs::input()),
                                        format!("with_{}", sanitize_shell_name(s)),
                                    ),
                                ]
                            }),
                        ), // package
                        Lit,  // help-string (can be a call to AS_HELP_STRING)
                        Cmds, // [actio-if-given]
                        Cmds, // [action-if-not-given]
                    ],
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
                                vec![(
                                    ExVar(VarAttrs::input()),
                                    format!("enable_{}", sanitize_shell_name(s)),
                                )]
                            }),
                        ), // feature
                        Lit,  // help-string
                        Cmds, // [action-if-given]
                        Cmds, // [action-if-not-given]
                    ],
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
                    ..Default::default()
                },
            ),
            (
                "PKG_PROG_PKG_CONFIG",
                M4MacroSignature {
                    arg_types: vec![
                        Word, // min-version
                    ],
                    ret_type: Some(Cmds),
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
                                    (ExVar(VarAttrs::output()), format!("{}_CFLAGS", s)),
                                    (ExVar(VarAttrs::output()), format!("{}_LIBS", s)),
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
                                    (ExVar(VarAttrs::output()), format!("{}_CFLAGS", s)),
                                    (ExVar(VarAttrs::output()), format!("{}_LIBS", s)),
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
                                    (ExVar(VarAttrs::output()), format!("{}_CFLAGS", s)),
                                    (ExVar(VarAttrs::output()), format!("{}_LIBS", s)),
                                    // argument
                                    (ExVar(VarAttrs::input()), format!("with_{}", s)),
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
                                    (ExVar(VarAttrs::output()), format!("{}_CFLAGS", s)),
                                    (ExVar(VarAttrs::output()), format!("{}_LIBS", s)),
                                    // argument & conditional
                                    (ExVar(VarAttrs::input()), format!("with_{}", s)),
                                    (ExAMCond, s.into()),
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
                                    (ExVar(VarAttrs::output()), format!("{}_CFLAGS", s)),
                                    (ExVar(VarAttrs::output()), format!("{}_LIBS", s)),
                                    // argument & conditional & cpp symbol
                                    (ExVar(VarAttrs::input()), format!("with_{}", s)),
                                    (ExAMCond, s.into()),
                                    (ExCPP, s.into()),
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
                        Var::define_precious("LT_SYS_LIBRARY_PATH"),
                        Var::define_input("with_gnu_ld"),
                        Var::define_input("with_gnu_ld"),
                        Var::define_precious("sysroot"),
                    ]),
                    ret_type: Some(Cmds),
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
                "AC_DISABLE_SHARED",
                // change the default libtool's behavior to disable shared libraries.
                M4MacroSignature {
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
                    ..Default::default()
                },
            ),
            (
                "LT_CMD_MAX_LEN",
                M4MacroSignature {
                    shell_vars: Some(vec!["max_cmd_len".into()]),
                    ..Default::default()
                },
            ),
            (
                "LT_FUNC_DLSYM_USCORE",
                M4MacroSignature {
                    cpp_symbols: Some(vec!["DLSYM_USCORE".into()]),
                    ..Default::default()
                },
            ),
            (
                "LT_LIB_M",
                M4MacroSignature {
                    shell_vars: Some(vec![Var::define_output("LIBM"), Var::add("LIBS", Output)]),
                    cpp_symbols: Some(vec!["HAVE_LIBM".into(), "HAVE_LIBMW".into()]),
                    require: Some(vec!["AC_CANONICAL_HOST".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_CHECK_LIBM",
                M4MacroSignature {
                    replaced_by: Some("LT_LIB_M".into()),
                    ..Default::default()
                },
            ),
            (
                "LT_LIB_DLLOAD",
                M4MacroSignature {
                    shell_vars: Some(vec![
                        Var::define_output("LT_DLLOADERS"),
                        Var::define_output("LIBADD_SHL_LOAD"),
                        Var::add("LIBS", Output),
                    ]),
                    cpp_symbols: Some(vec![
                        "HAVE_LIBDL".into(),
                        "HAVE_SHL_LOAD".into(),
                        "HAVE_DYLD".into(),
                        "HAVE_DLD".into(),
                    ]),
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
                    ..Default::default()
                },
            ),
            (
                "AC_PROG_NM", // obsolete macro
                M4MacroSignature {
                    replaced_by: Some("LT_PATH_NM".into()),
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
                        Var::add("LIBS", Output),
                    ]),
                    cpp_symbols: Some(vec!["HAVE_DLFCN_H".into()]),
                    paths: Some(vec!["dlfcn.h".into()]),
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
                    shell_vars: Some(vec!["lt_cv_sys_dlopen_deplibs".into()]),
                    cpp_symbols: Some(vec!["LTDL_DLOPEN_DEPLIBS".into()]),
                    require: Some(vec!["AC_CANONICAL_HOST".into()]),
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
                        "lt_cv_sys_dlsearch_path".into(),
                        "sys_dlsearch_path".into(),
                    ]),
                    cpp_symbols: Some(vec!["LTDL_DLSERCH_PATH".into()]),
                    ..Default::default()
                },
            ),
            (
                "AC_LTDL_SYSSEARCHPATH", // obsolete macro
                M4MacroSignature {
                    replaced_by: Some("LT_SYS_DLSEARCH_PATH".into()),
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
        ]
        .map(|(name, t)| (name.to_string(), t)),
    )
}
