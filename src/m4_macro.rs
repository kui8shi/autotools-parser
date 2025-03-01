//! Provide data structures related to m4 macros.
use std::collections::HashMap;
use M4Type::*;

/// Specify types of arguments or expansion of m4 macro calls.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum M4Type {
    /// maybe it's equivalent to a top level word in this program
    Lit,
    /// array of literals separated by whitespace
    Arr,
    /// array of macro arguments separated by comma
    Args,
    /// represents a program to be used for checking by compiling it
    Prog,
    /// list of shell script or m4 macro
    Cmds,
    /// result of macro definition
    Def,
    /// body of macro definition
    Body,
}

/// Represents an argument of m4 macro call.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum M4Argument<W, C> {
    /// raw literal
    Literal(String),
    /// array of words
    Array(Vec<W>),
    /// program string
    Program(String),
    /// list of commands.
    Command(Vec<C>),
    /// list of words
    Words(Vec<W>),
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
}

lazy_static::lazy_static! {
    /// Predefined m4/autoconf macros
    pub static ref MACROS
        : HashMap<String, (Vec<M4Type>, M4Type, Option<(usize, usize)>)>
        = HashMap::from([
        // Initializing configure
        ("AC_INIT", (vec![Lit, Lit, Lit, Lit, Lit], Cmds, None)),
        // Dealing with Autoconf versions
        ("AC_PREREQ", (vec![Lit], Cmds, None)),
        ("AC_AUTOCONF_VERSION", (vec![], Lit, None)),
        // Notices in configure
        ("AC_COPYRIGHT", (vec![Lit], Cmds, None)),
        ("AC_REVISION", (vec![Lit], Cmds, None)),
        // Configure input
        ("AC_CONFIG_SRCDIR", (vec![Lit], Cmds, None)),
        ("AC_CONFIG_MACRO_DIR", (vec![Lit], Cmds, None)),
        ("AC_CONFIG_MACRO_DIRS", (vec![Arr], Cmds, None)),
        ("AC_CONFIG_AUX_DIR", (vec![Lit], Cmds, None)),
        ("AC_CONFIG_AUX_FILE", (vec![Lit], Cmds, None)),
        // Outputting files
        ("AC_OUTPUT", (vec![], Cmds, None)),
        ("AC_PROG_MAKE_SET", (vec![], Cmds, None)),
        // Creating configuration files
        ("AC_CONFIG_FILES", (vec![Arr, Cmds, Cmds], Cmds, None)),
        // Configuration header files
        ("AC_CONFIG_HEADERS", (vec![Arr, Cmds, Cmds], Cmds, None)),
        ("AH_HEADER", (vec![], Lit, None)),
        ("AH_TEMPLATE", (vec![Lit, Lit], Cmds, None)),
        ("AH_VERBATIM", (vec![Lit, Lit], Cmds, None)),
        ("AH_TOP", (vec![Lit], Cmds, None)),
        ("AH_BOTTOM", (vec![Lit], Cmds, None)),
        // Running arbitrary configuration commands
        ("AC_CONFIG_COMMANDS", (vec![Arr, Cmds, Cmds], Cmds, None)),
        ("AH_CONFIG_COMMANDS_PRE", (vec![Cmds], Cmds, None)),
        ("AH_CONFIG_COMMANDS_POST", (vec![Cmds], Cmds, None)),
        // Creating configuration links
        ("AC_CONFIG_LINKS", (vec![Arr, Cmds, Cmds], Cmds, None)),
        // Configuring other packages in subdirectories
        ("AM_CONFIG_SUBDIRS", (vec![Arr], Cmds, None)),
        // Default prefix
        ("AM_PREFIX_DEFAULT", (vec![Lit], Cmds, None)),
        ("AM_PREFIX_PROGRAM", (vec![Lit], Cmds, None)),
        ("AM_CONDITIONAL", (vec![Lit, Cmds], Cmds, None)),
        // Default includes
        ("AC_INCLUDES_DEFAULT", (vec![Prog], Prog, None)),
        ("AC_CHECK_INCLUDES_DEFAULT", (vec![], Cmds, None)),
        // paticular program checks
        ("AC_PROG_AR", (vec![], Cmds, None)),
        ("AC_PROG_AWK", (vec![], Cmds, None)),
        ("AC_PROG_GREP", (vec![], Cmds, None)),
        ("AC_PROG_EGREP", (vec![], Cmds, None)),
        ("AC_PROG_FGREP", (vec![], Cmds, None)),
        ("AC_PROG_INSTALL", (vec![], Cmds, None)),
        ("AC_PROG_MKDIR_P", (vec![], Cmds, None)),
        ("AC_PROG_LEX", (vec![], Cmds, None)),
        ("AC_PROG_LN_S", (vec![], Cmds, None)),
        ("AC_PROG_RANLIB", (vec![], Cmds, None)),
        ("AC_PROG_SED", (vec![], Cmds, None)),
        ("AC_PROG_YACC", (vec![], Cmds, None)),
        // generic program and file checks
        ("AC_CHECK_PROG", (vec![Lit, Lit, Lit, Lit, Lit, Lit], Cmds, None)),
        ("AC_CHECK_PROGS", (vec![Lit, Arr, Lit, Lit, Lit, Lit], Cmds, None)),
        ("AC_CHECK_TARGET_TOOL", (vec![Lit, Lit, Lit, Lit], Cmds, None)),
        ("AC_CHECK_TOOL", (vec![Lit, Lit, Lit, Lit], Cmds, None)),
        ("AC_CHECK_TARGET_TOOLS", (vec![Lit, Arr, Lit, Lit], Cmds, None)),
        ("AC_CHECK_TOOLS", (vec![Lit, Arr, Lit, Lit], Cmds, None)),
        ("AC_PATH_PROG", (vec![Lit, Lit, Lit, Lit], Cmds, None)),
        ("AC_PATH_PROGS", (vec![Lit, Arr, Lit, Lit], Cmds, None)),
        (
            "AC_PATH_PROGS_FEATURE_CHECK",
            (vec![Lit, Arr, Cmds, Cmds, Lit], Cmds, None),
        ),
        ("AC_PATH_TARGET_TOOL", (vec![Lit, Lit, Lit, Lit], Cmds, None)),
        ("AC_PATH_TOOL", (vec![Lit, Lit, Lit, Lit], Cmds, None)),
        // Files
        ("AC_CHECK_FILE", (vec![Lit, Cmds, Cmds], Cmds, None)),
        ("AC_CHECK_FILES", (vec![Arr, Cmds, Cmds], Cmds, None)),
        // Library files
        ("AC_CHECK_LIB", (vec![Lit, Lit, Cmds, Cmds, Arr], Cmds, None)),
        ("AC_SEARCH_LIBS", (vec![Lit, Arr, Cmds, Cmds, Arr], Cmds, None)),
        // Paticular function checks
        ("AC_FUNC_ALLOCA", (vec![], Cmds, None)),
        ("AC_FUNC_CHOWN", (vec![], Cmds, None)),
        ("AC_FUNC_CLOSEDIR_VOID", (vec![], Cmds, None)),
        ("AC_FUNC_ERROR_AT_LINE", (vec![], Cmds, None)),
        ("AC_FUNC_FNMATCH", (vec![], Cmds, None)),
        ("AC_FUNC_FNMATCH_GNU", (vec![], Cmds, None)),
        ("AC_FUNC_FORK", (vec![], Cmds, None)),
        ("AC_FUNC_FSEEKO", (vec![], Cmds, None)),
        ("AC_FUNC_GETGROUPS", (vec![], Cmds, None)),
        ("AC_FUNC_GETLOADAVG", (vec![], Cmds, None)),
        ("AC_FUNC_GETMNTENT", (vec![], Cmds, None)),
        ("AC_FUNC_GETPGRP", (vec![], Cmds, None)),
        ("AC_FUNC_LSTAT_FOLLOWS_SLASHED_SYMLINK", (vec![], Cmds, None)),
        ("AC_FUNC_MALLOC", (vec![], Cmds, None)),
        ("AC_FUNC_MBRTOWC", (vec![], Cmds, None)),
        ("AC_FUNC_MEMCMP", (vec![], Cmds, None)),
        ("AC_FUNC_MKTIME", (vec![], Cmds, None)),
        ("AC_FUNC_MMAP", (vec![], Cmds, None)),
        ("AC_FUNC_OBSTACK", (vec![], Cmds, None)),
        ("AC_FUNC_REALLOC", (vec![], Cmds, None)),
        ("AC_FUNC_SELECT_ARGTYPES", (vec![], Cmds, None)),
        ("AC_FUNC_SETPGRP", (vec![], Cmds, None)),
        ("AC_FUNC_STAT", (vec![], Cmds, None)),
        ("AC_FUNC_LSTAT", (vec![], Cmds, None)),
        ("AC_FUNC_STRCOLL", (vec![], Cmds, None)),
        ("AC_FUNC_STRERROR_R", (vec![], Cmds, None)),
        ("AC_FUNC_STRFTIME", (vec![], Cmds, None)),
        ("AC_FUNC_STRTOD", (vec![], Cmds, None)),
        ("AC_FUNC_STRTOLD", (vec![], Cmds, None)),
        ("AC_FUNC_STRNLEN", (vec![], Cmds, None)),
        ("AC_FUNC_UTIME_NULL", (vec![], Cmds, None)),
        ("AC_FUNC_VPRINTF", (vec![], Cmds, None)),
        ("AC_REPLACE_FNMATCH", (vec![], Cmds, None)),
        // Generic function checks
        ("AC_CHECK_FUNC", (vec![Lit, Cmds, Cmds], Cmds, None)),
        ("AC_CHECK_FUNCS", (vec![Arr, Cmds, Cmds], Cmds, None)),
        ("AC_CHECK_FUNCS_ONCE", (vec![Arr], Cmds, None)),
        ("AC_LIBOBJ", (vec![Lit], Cmds, None)),
        ("AC_LIBSOURCE", (vec![Lit], Cmds, None)),
        ("AC_LIBSOURCES", (vec![Arr], Cmds, None)),
        ("AC_CONFIG_LIBOBJ_DIR", (vec![Lit], Cmds, None)),
        ("AC_REPLACE_FUNCS", (vec![Arr], Cmds, None)),
        // Paticular header checks
        ("AC_CHECK_HEADER_STDBOOL", (vec![], Cmds, None)),
        ("AC_HEADER_ASSERT", (vec![], Cmds, None)),
        ("AC_HEADER_DIRENT", (vec![], Cmds, None)),
        ("AC_HEADER_MAJOR", (vec![], Cmds, None)),
        ("AC_HEADER_RESOLV", (vec![], Cmds, None)),
        ("AC_HEADER_STAT", (vec![], Cmds, None)),
        ("AC_HEADER_STDBOOL", (vec![], Cmds, None)),
        ("AC_HEADER_STDC", (vec![], Cmds, None)),
        ("AC_HEADER_SYS_WAIT", (vec![], Cmds, None)),
        ("AC_HEADER_TIOCGWINSZ", (vec![], Cmds, None)),
        // Generic header checks
        ("AC_CHECK_HEADER", (vec![Lit, Cmds, Cmds], Cmds, None)),
        ("AC_CHECK_HEADERS", (vec![Arr, Cmds, Cmds], Cmds, None)),
        ("AC_CHECK_HEADERS_ONCE", (vec![Arr], Cmds, None)),
        // Generic declaration checks
        ("AC_CHECK_DECL", (vec![Lit, Cmds, Cmds, Prog], Cmds, None)),
        ("AC_CHECK_DECLS", (vec![Arr, Cmds, Cmds, Prog], Cmds, None)),
        ("AC_CHECK_DECLS_ONCE", (vec![Arr], Cmds, None)),
        // Paticular structure checks
        ("AC_STRUCT_DIRENT_D_INO", (vec![], Cmds, None)),
        ("AC_STRUCT_DIRENT_D_TYPE", (vec![], Cmds, None)),
        ("AC_STRUCT_ST_BLOCKS", (vec![], Cmds, None)),
        ("AC_STRUCT_TM", (vec![], Cmds, None)),
        ("AC_STRUCT_TIMEZONE", (vec![], Cmds, None)),
        // Generic structure checks
        ("AC_STRUCT_MEMBER", (vec![Lit, Cmds, Cmds, Cmds], Cmds, None)),
        ("AC_STRUCT_MEMBERS", (vec![Arr, Cmds, Cmds, Cmds], Cmds, None)),
        // Paticular type checks
        ("AC_TYPE_GETGROUPS", (vec![], Cmds, None)),
        ("AC_TYPE_INT8_T", (vec![], Cmds, None)),
        ("AC_TYPE_INT16_T", (vec![], Cmds, None)),
        ("AC_TYPE_INT32_T", (vec![], Cmds, None)),
        ("AC_TYPE_INT64_T", (vec![], Cmds, None)),
        ("AC_TYPE_INTMAX_T", (vec![], Cmds, None)),
        ("AC_TYPE_INTPTR_T", (vec![], Cmds, None)),
        ("AC_TYPE_LONG_DOUBLE", (vec![], Cmds, None)),
        ("AC_TYPE_LONG_DOUBLE_WIDER", (vec![], Cmds, None)),
        ("AC_TYPE_LONG_LONG_INT", (vec![], Cmds, None)),
        ("AC_TYPE_MBSTATE_T", (vec![], Cmds, None)),
        ("AC_TYPE_MODE_T", (vec![], Cmds, None)),
        ("AC_TYPE_OFF_T", (vec![], Cmds, None)),
        ("AC_TYPE_PID_T", (vec![], Cmds, None)),
        ("AC_TYPE_SIZE_T", (vec![], Cmds, None)),
        ("AC_TYPE_SSIZE_T", (vec![], Cmds, None)),
        ("AC_TYPE_UID_T", (vec![], Cmds, None)),
        ("AC_TYPE_UINT8_T", (vec![], Cmds, None)),
        ("AC_TYPE_UINT16_T", (vec![], Cmds, None)),
        ("AC_TYPE_UINT32_T", (vec![], Cmds, None)),
        ("AC_TYPE_UINT64_T", (vec![], Cmds, None)),
        ("AC_TYPE_UINTMAX_T", (vec![], Cmds, None)),
        ("AC_TYPE_UINTPTR_T", (vec![], Cmds, None)),
        ("AC_TYPE_UNSIGNED_LONG_LONG_T", (vec![], Cmds, None)),
        // Generic type checks
        ("AC_CHECK_TYPE", (vec![Lit, Cmds, Cmds, Prog], Cmds, None)),
        ("AC_CHECK_TYPES", (vec![Arr, Cmds, Cmds, Prog], Cmds, None)),
        // Generic compiler characteristics
        ("AC_CHECK_SIZEOF", (vec![Lit, Lit, Prog], Cmds, None)),
        ("AC_CHECK_ALIGNOF", (vec![Lit, Prog], Cmds, None)),
        ("AC_COMPUTE_INT", (vec![Lit, Lit, Cmds], Cmds, None)),
        ("AC_LANG_WERROR", (vec![], Cmds, None)),
        ("AC_OPENMP", (vec![], Cmds, None)),
        ("AC_PROG_CC", (vec![Arr], Cmds, None)),
        ("AC_PROG_CC_C_O", (vec![], Cmds, None)),
        ("AC_PROG_CPP", (vec![], Cmds, None)),
        ("AC_PROG_CPP_WERROR", (vec![], Cmds, None)),
        ("AC_C_BACKSLASH_A", (vec![], Cmds, None)),
        ("AC_C_BIGENDIAN", (vec![], Cmds, None)),
        ("AC_C_CONST", (vec![], Cmds, None)),
        ("AC_C__GENERIC", (vec![], Cmds, None)),
        ("AC_C_RESTRICT", (vec![], Cmds, None)),
        ("AC_C_VOLATILE", (vec![], Cmds, None)),
        ("AC_C_INLINE", (vec![], Cmds, None)),
        ("AC_C_CHAR_UNSIGNED", (vec![], Cmds, None)),
        ("AC_C_STRINGIZE", (vec![], Cmds, None)),
        ("AC_C_FLEXIBLE_ARRAY_MEMBER", (vec![], Cmds, None)),
        ("AC_C_VARARRAYS", (vec![], Cmds, None)),
        ("AC_C_TYPEOF", (vec![], Cmds, None)),
        ("AC_C_PROTOTYPES", (vec![], Cmds, None)),
        // C++ compiler characteristics
        ("AC_PROG_CXX", (vec![Arr], Cmds, None)),
        ("AC_PROG_CXXCPP", (vec![], Cmds, None)),
        ("AC_PROG_CXX_C_O", (vec![], Cmds, None)),
        // Objective C compiler characteristics
        ("AC_PROG_OBJC", (vec![Arr], Cmds, None)),
        ("AC_PROG_OBJCPP", (vec![], Cmds, None)),
        // Objective C compiler characteristics
        ("AC_PROG_OBJCXX", (vec![Arr], Cmds, None)),
        ("AC_PROG_OBJCXXCPP", (vec![], Cmds, None)),
        // Erlang compiler and interpreter characteristics
        ("AC_ERLANG_PATH_ERLC", (vec![Lit, Lit], Cmds, None)),
        ("AC_ERLANG_NEED_ERLC", (vec![Lit], Cmds, None)),
        ("AC_ERLANG_PATH_ERL", (vec![Lit, Lit], Cmds, None)),
        ("AC_ERLANG_NEED_ERL", (vec![Lit], Cmds, None)),
        // Fortran compiler characteristics
        ("AC_PROG_F77", (vec![Arr], Cmds, None)),
        ("AC_PROG_FC", (vec![Arr, Lit], Cmds, None)),
        ("AC_PROG_F77_C_O", (vec![], Cmds, None)),
        ("AC_PROG_FC_C_O", (vec![], Cmds, None)),
        ("AC_F77_LIBRARY_LDFLAGS", (vec![], Cmds, None)),
        ("AC_FC_LIBRARY_LDFLAGS", (vec![], Cmds, None)),
        ("AC_F77_DUMMY_MAIN", (vec![Cmds, Cmds, Cmds], Cmds, None)),
        ("AC_FC_DUMMY_MAIN", (vec![Cmds, Cmds, Cmds], Cmds, None)),
        ("AC_F77_MAIN", (vec![], Cmds, None)),
        ("AC_FC_MAIN", (vec![], Cmds, None)),
        ("AC_F77_WRAPPERS", (vec![], Cmds, None)),
        ("AC_FC_WRAPPERS", (vec![], Cmds, None)),
        ("AC_F77_FUNC", (vec![Lit, Lit], Cmds, None)),
        ("AC_FC_FUNC", (vec![Lit, Lit], Cmds, None)),
        ("AC_FC_SRCEXT", (vec![Lit, Cmds, Cmds], Cmds, None)),
        ("AC_FC_PP_SRCEXT", (vec![Lit, Cmds, Cmds], Cmds, None)),
        ("AC_FC_PP_DEFINE", (vec![Cmds, Cmds], Cmds, None)),
        ("AC_FC_FIXEDFORM", (vec![Cmds, Cmds], Cmds, None)),
        ("AC_FC_LINE_LENGTH", (vec![Lit, Cmds, Cmds], Cmds, None)),
        ("AC_FC_CHECK_LENGTH", (vec![Cmds, Cmds], Cmds, None)),
        ("AC_F77_IMPLICIT_NONE", (vec![Cmds, Cmds], Cmds, None)),
        ("AC_FC_IMPLICIT_NONE", (vec![Cmds, Cmds], Cmds, None)),
        ("AC_FC_MODULE_EXTENSION", (vec![], Cmds, None)),
        ("AC_FC_MODULE_FLAG", (vec![Cmds, Cmds], Cmds, None)),
        ("AC_FC_MODULE_OUTPUT_FLAG", (vec![Cmds, Cmds], Cmds, None)),
        ("AC_F77_CRAY_POINTERS", (vec![Cmds, Cmds], Cmds, None)),
        ("AC_FC_CRAY_POINTERS", (vec![Cmds, Cmds], Cmds, None)),
        // Go compiler characteristics
        ("AC_PROG_GO", (vec![Arr], Cmds, None)),
        // System services
        ("AC_PATH_X", (vec![], Cmds, None)),
        ("AC_PATH_XTRA", (vec![], Cmds, None)),
        ("AC_SYS_INTERPRETER", (vec![], Cmds, None)),
        ("AC_SYS_LARGEFILE", (vec![], Cmds, None)),
        ("AC_SYS_LONG_FILE_NAMES", (vec![], Cmds, None)),
        ("AC_SYS_POSIX_TERMIOS", (vec![], Cmds, None)),
        ("AC_SYS_YEAR2038", (vec![], Cmds, None)),
        ("AC_SYS_YEAR2038_RECOMMENDED", (vec![], Cmds, None)),
        // C and posix variants
        ("AC_USE_SYSTEM_EXTENSIONS", (vec![], Cmds, None)),
        // Erlang libraries
        ("AC_ERLANG_SUBST_ERTS_VER", (vec![], Cmds, None)),
        ("AC_ERLANG_SUBST_ROOT_DIR", (vec![], Cmds, None)),
        ("AC_ERLANG_SUBST_LIB_DIR", (vec![], Cmds, None)),
        ("AC_ERLANG_CHECK_LIB", (vec![Lit, Cmds, Cmds], Cmds, None)),
        ("AC_ERLANG_SUBST_INSTALL_LIB_DIR", (vec![], Cmds, None)),
        ("AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR", (vec![Lit, Lit], Cmds, None)),
        // Language choice
        ("AC_LANG", (vec![Lit], Cmds, None)),
        ("AC_LANG_PUSH", (vec![Lit], Cmds, None)),
        ("AC_LANG_POP", (vec![Lit], Cmds, None)),
        ("AC_LANG_ASSERT", (vec![Lit], Cmds, None)),
        ("AC_REQUIRE_CPP", (vec![], Cmds, None)),
        // Generating sources
        ("AC_LANG_CONFTEST", (vec![Lit], Cmds, None)),
        ("AC_LANG_DEFINES_PROVIDED", (vec![], Cmds, None)),
        ("AC_LANG_SOURCE", (vec![Prog], Prog, None)),
        ("AC_LANG_PROGRAM", (vec![Prog, Prog], Prog, None)),
        ("AC_LANG_CALL", (vec![Prog, Prog], Prog, None)),
        ("AC_LANG_FUNC_LINK_TRY", (vec![Prog], Prog, None)),
        // Running the preprocessor
        ("AC_PREPROC_IFELSE", (vec![Prog, Cmds, Cmds], Cmds, None)),
        ("AC_EGREP_HEADER", (vec![Lit, Prog, Cmds, Cmds], Cmds, None)),
        ("AC_EGREP_CPP", (vec![Lit, Prog, Cmds, Cmds], Cmds, None)),
        // Running the compiler
        ("AC_COMPILE_IFELSE", (vec![Prog, Cmds, Cmds], Cmds, None)),
        // Running the linker
        ("AC_LINK_IFELSE", (vec![Prog, Cmds, Cmds], Cmds, None)),
        // Checking runtime behavior
        ("AC_RUN_IFELSE", (vec![Prog, Cmds, Cmds, Cmds], Cmds, None)),
        // Defininig C preprocessor symbols
        ("AC_DEFINE", (vec![Lit, Lit, Lit], Def, None)),
        ("AC_DEFINE_UNQUOTED", (vec![Lit, Lit, Lit], Def, None)),
        // Setting output variables
        ("AC_SUBST", (vec![Lit, Lit], Cmds, None)),
        ("AC_SUBST_FILE", (vec![Lit], Cmds, None)),
        ("AC_ARG_VAR", (vec![Lit], Cmds, None)),
        // Caching results
        ("AC_CACHE_VAL", (vec![Lit, Cmds], Cmds, None)),
        ("AC_CACHE_CHECK", (vec![Lit, Lit, Cmds], Cmds, None)),
        ("AC_CACHE_LOAD", (vec![], Cmds, None)),
        ("AC_CACHE_SAVE", (vec![], Cmds, None)),
        // Printing messages
        ("AC_MSG_CHECKING", (vec![Lit], Cmds, None)),
        ("AC_MSG_RESULT", (vec![Lit], Cmds, None)),
        ("AC_MSG_NOTICE", (vec![Lit], Cmds, None)),
        ("AC_MSG_ERROR", (vec![Lit, Lit], Cmds, None)),
        ("AC_MSG_FAILURE", (vec![Lit, Lit], Cmds, None)),
        ("AC_MSG_WARN", (vec![Lit], Cmds, None)),
        // Redefined M4 macros
        ("m4_builtin", (vec![Lit], Cmds, None)),
        ("m4_changecom", (vec![Lit], Cmds, None)),
        ("m4_changequote", (vec![Lit], Cmds, None)),
        ("m4_debugfile", (vec![Lit], Cmds, None)),
        ("m4_debugmode", (vec![Lit], Cmds, None)),
        ("m4_decr", (vec![Lit], Cmds, None)),
        ("m4_define", (vec![Lit, Lit], Def, None)), // The command parsing is delayed till expandsion
        ("m4_defun", (vec![Lit, Lit], Def, None)),  // The command parsing is delayed till expandsion
        ("define", (vec![Lit, Lit], Def, None)),  // The command parsing is delayed till expandsion
        ("m4_divnum", (vec![Lit], Cmds, None)),
        ("m4_errprint", (vec![Lit], Cmds, None)),
        ("m4_esyscmd", (vec![Lit], Cmds, None)),
        ("m4_eval", (vec![Lit], Cmds, None)),
        ("m4_format", (vec![Lit], Cmds, None)),
        ("m4_ifdef", (vec![Lit], Cmds, None)),
        ("m4_incr", (vec![Lit], Cmds, None)),
        ("m4_index", (vec![Lit], Cmds, None)),
        ("m4_indir", (vec![Lit], Cmds, None)),
        ("m4_len", (vec![Lit], Cmds, None)),
        ("m4_pushdef", (vec![Lit, Lit], Def, None)),
        ("m4_shift", (vec![Args], Args, None)),
        ("m4_substr", (vec![Lit, Lit, Lit], Lit, None)),
        ("m4_syscmd", (vec![Lit], Cmds, None)),
        ("m4_sysval", (vec![Lit], Cmds, None)),
        ("m4_traceoff", (vec![Lit], Cmds, None)),
        ("m4_traceon", (vec![Lit], Cmds, None)),
        ("m4_translit", (vec![Lit, Lit, Lit], Lit, None)),
        ("__file__", (vec![], Lit, None)),
        ("__line__", (vec![], Lit, None)),
        ("__oline__", (vec![], Lit, None)),
        ("patsubst", (vec![Lit, Lit, Lit], Lit, None)),
        ("regexp", (vec![Lit, Lit, Lit], Lit, None)),
        ("m4_bpatsubst", (vec![Lit, Lit, Lit], Lit, None)), // originally it is `patsubst` in m4
        ("m4_bregexp", (vec![Lit, Lit, Lit], Lit, None)), // originally it is `regexp` in m4
        ("m4_copy", (vec![Lit, Lit], Cmds, None)),
        ("m4_copy_force", (vec![Lit, Lit], Cmds, None)),
        ("m4_rename", (vec![Lit, Lit], Cmds, None)),
        ("m4_rename_force", (vec![Lit, Lit], Cmds, None)),
        ("m4_defn", (vec![Arr], Cmds, None)),
        ("m4_divert", (vec![Lit], Cmds, None)),
        ("m4_dumpdef", (vec![Arr], Cmds, None)),
        ("m4_dumpdefs", (vec![Arr], Cmds, None)),
        ("m4_esyscmd_s", (vec![Lit], Cmds, None)),
        ("m4_exit", (vec![Lit], Cmds, None)),
        (
            "m4_if",
            (vec![Lit, Lit, Cmds, Cmds], Cmds, Some((0, 3))),
        ), //varargs
        ("m4_include", (vec![Lit], Cmds, None)),
        ("m4_sinclude", (vec![Lit], Cmds, None)),
        ("m4_mkstemp", (vec![Lit], Cmds, None)),
        ("m4_maketemp", (vec![Lit], Cmds, None)),
        ("m4_popdef", (vec![Arr], Cmds, None)),
        ("m4_undefine", (vec![Arr], Def, None)),
        ("m4_undivert", (vec![Arr], Cmds, None)),
        ("m4_wrap", (vec![Lit], Cmds, None)),
        ("m4_wrap_lifo", (vec![Lit], Cmds, None)),
        ("m4_assert", (vec![Lit], Cmds, None)),
        ("m4_errprintn", (vec![Lit], Cmds, None)),
        ("m4_fatal", (vec![Lit], Cmds, None)),
        ("m4_location", (vec![], Lit, None)),
        ("m4_warn", (vec![Lit, Lit], Lit, None)),
        ("m4_cleandivert", (vec![Arr], Cmds, None)),
        ("m4_divert_once", (vec![Lit, Lit], Cmds, None)),
        ("m4_divert_pop", (vec![Lit], Cmds, None)),
        ("m4_divert_push", (vec![Lit], Cmds, None)),
        ("m4_divert_text", (vec![Lit, Lit], Cmds, None)),
        ("m4_init", (vec![], Cmds, None)),
        (
            "m4_bmatch",
            (vec![Lit, Lit, Lit, Lit], Lit, Some((1, 3))),
        ), // varargs
        (
            "m4_bpatsubsts",
            (vec![Lit, Lit, Lit, Lit], Lit, Some((1, 3))),
        ), // varargs
        ("m4_case", (vec![Lit, Lit, Lit, Lit], Lit, Some((1, 3)))), // varargs
        ("m4_cond", (vec![Lit, Lit, Lit, Lit], Lit, Some((0, 3)))), // varargs
        ("m4_default", (vec![Lit, Lit], Lit, None)),
        ("m4_default_quoted", (vec![Lit, Lit], Lit, None)),
        ("m4_default_nblank", (vec![Lit, Lit], Lit, None)),
        ("m4_default_nblank_quoted", (vec![Lit, Lit], Lit, None)),
        ("m4_define_default", (vec![Lit, Cmds], Cmds, None)),
        ("m4_ifblank", (vec![Lit, Cmds, Cmds], Cmds, None)),
        ("m4_ifnblank", (vec![Lit, Cmds, Cmds], Cmds, None)),
        ("m4_ifndef", (vec![Lit, Cmds, Cmds], Cmds, None)),
        ("m4_ifset", (vec![Lit, Cmds, Cmds], Cmds, None)),
        ("m4_ifval", (vec![Lit, Cmds, Cmds], Cmds, None)),
        ("m4_ifvaln", (vec![Lit, Cmds, Cmds], Cmds, None)),
        ("m4_n", (vec![Lit], Lit, None)),
        ("m4_argn", (vec![Lit, Lit], Lit, Some((1, 2)))), //varargs
        ("m4_car", (vec![Lit], Lit, None)),       //varargs
        ("m4_cdr", (vec![Lit], Lit, Some((0, 1)))),       //varargs
        ("m4_for", (vec![Lit, Lit, Arr, Cmds], Cmds, None)),
        ("m4_foreach", (vec![Lit, Arr, Cmds], Cmds, None)),
        ("m4_foreach_w", (vec![Lit, Arr, Cmds], Cmds, None)),
        ("m4_map", (vec![Lit, Arr], Cmds, None)), //Actually return type is unknown
        ("m4_mapall", (vec![Lit, Arr], Cmds, None)),
        ("m4_map_sep", (vec![Lit, Lit, Arr], Cmds, None)),
        ("m4_mapall_sep", (vec![Lit, Lit, Arr], Cmds, None)),
        ("m4_map_args", (vec![Lit, Lit], Cmds, None)),
        ("m4_map_args_pair", (vec![Lit, Lit, Lit], Cmds, None)),
        ("m4_map_args_sep", (vec![Lit, Lit, Lit, Lit], Cmds, None)),
        ("m4_map_args_w", (vec![Lit, Lit, Lit, Lit], Cmds, None)),
        ("m4_shiftn", (vec![Lit, Lit], Cmds, Some((1, 2)))), // varargs
        ("m4_shift2", (vec![Lit], Cmds, Some((0, 1)))),      // varargs
        ("m4_shift3", (vec![Lit], Cmds, Some((0, 1)))),      // varargs
        ("m4_stack_foreach", (vec![Lit, Cmds], Cmds, None)),
        ("m4_stack_foreach_lifo", (vec![Lit, Cmds], Cmds, None)),
        ("m4_stack_foreach_sep", (vec![Lit, Cmds, Cmds, Lit], Cmds, None)),
        (
            "m4_stack_foreach_sep_lifo",
            (vec![Lit, Cmds, Cmds, Lit], Cmds, None),
        ),
        ("m4_apply", (vec![Lit, Arr], Cmds, None)),
        ("m4_curry", (vec![Lit, Lit], Lit, None)), // returns Macro
        ("m4_do", (vec![Lit], Lit, Some((0, 1)))), //varargs
        ("m4_dquote", (vec![Lit], Arr, Some((0, 1)))), // varargs
        ("m4_dquote_elt", (vec![Lit], Arr, Some((0, 1)))), // varargs
        ("m4_echo", (vec![Lit], Lit, Some((0, 1)))), // varargs
        ("m4_expand", (vec![Lit], Lit, None)),
        ("m4_ignore", (vec![Lit], Lit, None)),
        ("m4_make_list", (vec![Lit], Arr, None)),
        ("m4_append", (vec![Lit, Lit, Lit], Lit, None)), //cannot define its return type
        ("m4_append_uniq", (vec![Lit, Lit, Lit, Lit, Lit], Lit, None)),
        ("m4_append_uniq_w", (vec![Lit, Arr], Lit, None)),
        ("m4_chomp", (vec![Lit], Lit, None)),
        ("m4_chomp_all", (vec![Lit], Lit, None)),
        ("m4_combine", (vec![Lit, Arr, Lit, Lit], Lit, None)),
        ("m4_escape", (vec![Lit], Lit, None)),
        ("m4_flatten", (vec![Lit], Lit, None)),
        ("m4_join", (vec![Lit, Lit], Lit, Some((1, 2)))), //varargs
        ("m4_joinall", (vec![Lit, Lit], Lit, Some((1, 2)))), //varargs
        ("m4_newline", (vec![Lit], Lit, None)),
        ("m4_normalize", (vec![Lit], Lit, None)),
        ("m4_re_escape", (vec![Lit], Lit, None)),
        ("m4_split", (vec![Lit, Lit], Arr, None)),
        ("m4_strip", (vec![Lit], Lit, None)),
        ("m4_text_box", (vec![Lit, Lit], Lit, None)),
        ("m4_text_wrap", (vec![Lit, Lit, Lit, Lit], Lit, None)),
        ("m4_tolower", (vec![Lit], Lit, None)),
        ("m4_toupper", (vec![Lit], Lit, None)),
        ("m4_cmp", (vec![Lit, Lit], Lit, None)),
        ("m4_list_cmp", (vec![Arr, Arr], Lit, None)),
        ("m4_max", (vec![Lit], Lit, Some((0, 1)))), //varargs
        ("m4_min", (vec![Lit], Lit, Some((0, 1)))), //varargs
        ("m4_sign", (vec![Lit], Lit, None)),
        ("m4_version_compare", (vec![Lit, Lit], Lit, None)),
        ("m4_version_prereq", (vec![Lit, Cmds, Cmds], Cmds, None)),
        ("m4_set_add", (vec![Lit, Lit, Cmds, Cmds], Cmds, None)),
        ("m4_set_contains", (vec![Lit, Lit, Cmds, Cmds], Cmds, None)),
        ("m4_set_contents", (vec![Lit, Lit], Lit, None)),
        ("m4_set_dump", (vec![Lit, Lit], Lit, None)),
        ("m4_set_delete", (vec![Lit], Cmds, None)),
        ("m4_set_difference", (vec![Lit, Lit], Arr, None)),
        ("m4_set_intersection", (vec![Lit, Lit], Arr, None)),
        ("m4_set_union", (vec![Lit, Lit], Arr, None)),
        ("m4_set_empty", (vec![Lit, Cmds, Cmds], Cmds, None)),
        ("m4_set_foreach", (vec![Lit, Lit, Cmds], Cmds, None)),
        ("m4_set_list", (vec![Lit], Arr, None)),
        ("m4_set_listc", (vec![Lit], Arr, None)),
        ("m4_set_map", (vec![Lit, Cmds], Cmds, None)),
        ("m4_set_map_sep", (vec![Lit, Cmds, Cmds, Lit], Cmds, None)),
        ("m4_set_size", (vec![Lit], Lit, None)),
        // Common shell constructs
        ("AS_BOX", (vec![Lit, Lit], Cmds, None)),
        (
            "AS_CASE",
            (vec![Lit, Lit, Cmds, Cmds], Cmds, Some((1, 3))),
        ), //varargs
        ("AS_DIRNAME", (vec![Lit], Lit, None)),
        ("AS_ECHO", (vec![Lit], Cmds, None)),
        ("AS_ECHO_N", (vec![Lit], Cmds, None)),
        ("AS_ESCAPE", (vec![Lit, Lit], Lit, None)),
        ("AS_EXECUTABLE_P", (vec![Lit], Cmds, None)),
        ("AS_EXIT", (vec![Lit], Cmds, None)),
        ("AS_IF", (vec![Cmds, Cmds, Cmds], Cmds, Some((0, 2)))), // varargs
        ("AS_MKDIR_P", (vec![Lit], Cmds, None)),
        ("AS_SET_STATUS", (vec![Lit], Cmds, None)),
        ("AS_TR_CPP", (vec![Lit], Prog, None)),
        ("AS_TR_SH", (vec![Lit], Cmds, None)),
        ("AS_SET_CATFILE", (vec![Lit, Lit, Lit], Cmds, None)),
        ("AS_UNSET", (vec![Lit], Cmds, None)),
        (
            "AS_VERSION_COMPARE",
            (vec![Lit, Lit, Cmds, Cmds, Cmds], Cmds, None),
        ),
        // Support for indirect variable names
        ("AS_LITERAL_IF", (vec![Lit, Cmds, Cmds, Cmds], Cmds, None)),
        ("AS_LITERAL_WORD_IF", (vec![Lit, Cmds, Cmds, Cmds], Cmds, None)),
        ("AS_VAR_APPEND", (vec![Lit, Lit], Cmds, None)),
        ("AS_VAR_ARITH", (vec![Lit, Lit], Cmds, None)),
        ("AS_VAR_COPY", (vec![Lit, Lit], Cmds, None)),
        ("AS_VAR_IF", (vec![Lit, Lit, Cmds, Cmds], Cmds, None)),
        ("AS_VAR_PUSHDEF", (vec![Lit, Lit], Cmds, None)),
        ("AS_VAR_POPDEF", (vec![Lit], Cmds, None)),
        ("AS_VAR_SET", (vec![Lit, Lit], Cmds, None)),
        ("AS_VAR_SET_IF", (vec![Lit, Cmds, Cmds], Cmds, None)),
        ("AS_VAR_TEST_SET", (vec![Lit], Cmds, None)),
        // Initialization macros
        ("AS_BOURNE_COMPATIBLE", (vec![], Cmds, None)),
        ("AS_INIT", (vec![], Cmds, None)),
        ("AS_INIT_GENERATED", (vec![Lit, Lit], Cmds, None)),
        ("AS_LINENO_PREPARE", (vec![], Cmds, None)),
        ("AS_ME_PREPARE", (vec![], Cmds, None)),
        ("AS_TMPDIR", (vec![Lit, Lit], Cmds, None)),
        ("AS_SHELL_SANITIZE", (vec![], Cmds, None)),
        // File descriptor macros
        ("AS_MESSAGE_FD", (vec![], Lit, None)),
        ("AS_MESSAGE_LOG_FD", (vec![], Lit, None)),
        ("AC_FD_CC", (vec![], Lit, None)), // deprecated
        ("AS_ORIGINAL_STDIN_FD", (vec![], Lit, None)),
        // Macro definitions
        ("AC_DEFUN", (vec![Lit, Lit], Cmds, None)), // The command parsing is delayed till expandsion
        // Prerequisite macros
        ("AC_REQUIRE", (vec![Lit], Cmds, None)),
        ("AC_BEFORE", (vec![Lit, Lit], Cmds, None)),
        // One-shot macros
        ("AC_DEFUN_ONCE", (vec![Lit, Lit], Cmds, None)),
        // Getting the canonical system type
        ("AC_CANONICAL_BUILD", (vec![], Cmds, None)),
        ("AC_CANONICAL_HOST", (vec![], Cmds, None)),
        ("AC_CANONICAL_TARGET", (vec![], Cmds, None)),
        // Site configuration
        ("AC_PRESERVE_HELP_ORDER", (vec![], Cmds, None)),
        ("AC_ARG_WITH", (vec![Lit, Lit, Cmds, Cmds], Cmds, None)),
        ("AC_ARG_ENABLE", (vec![Lit, Lit, Cmds, Cmds], Cmds, None)),
        ("AS_HELP_STRING", (vec![Lit, Lit], Lit, None)),
        ("AC_DISABLE_OPTION_CHECKING", (vec![], Cmds, None)),
        ("AC_ARG_PROGRAM", (vec![], Cmds, None)),
        ("AC_ARG_PROGRAM", (vec![], Cmds, None)),
        // Generating test suites with Autotest
        ("AT_INIT", (vec![Lit], Cmds, None)),
        ("AT_COPYRIGHT", (vec![Lit], Cmds, None)),
        ("AT_ARG_OPTION", (vec![Arr, Lit, Cmds, Cmds], Cmds, None)),
        ("AT_ARG_OPTION_ARG", (vec![Arr, Lit, Cmds, Cmds], Cmds, None)),
        ("AT_COLOR_TESTS", (vec![], Cmds, None)),
        ("AT_TESTED", (vec![Arr], Cmds, None)),
        ("AT_PREPARE_TESTS", (vec![Cmds], Cmds, None)),
        ("AT_PREPARE_EACH_TEST", (vec![Cmds], Cmds, None)),
        ("AT_TEST_HELPER_FN", (vec![Lit, Lit, Lit, Cmds], Cmds, None)),
        ("AT_BANNER", (vec![Lit], Cmds, None)),
        ("AT_SETUP", (vec![Lit], Cmds, None)),
        ("AT_KEYWORDS", (vec![Arr], Cmds, None)),
        ("AT_CAPTURE_FILE", (vec![Lit], Cmds, None)),
        ("AT_FAIL_IF", (vec![Cmds], Cmds, None)),
        ("AT_SKIP_IF", (vec![Cmds], Cmds, None)),
        ("AT_XFAIL_IF", (vec![Cmds], Cmds, None)),
        ("AT_CLEANUP", (vec![], Cmds, None)),
        ("AT_DATA", (vec![Lit, Lit], Cmds, None)),
        ("AT_DATA_UNQUOTED", (vec![Lit, Lit], Cmds, None)),
        ("AT_CHECK", (vec![Cmds, Lit, Lit, Lit, Cmds, Cmds], Cmds, None)),
        (
            "AT_CHECK_UNQUOTED",
            (vec![Cmds, Lit, Lit, Lit, Cmds, Cmds], Cmds, None),
        ),
        ("AT_CHECK_EUNIT", (vec![Lit, Lit, Lit, Cmds, Cmds], Cmds, None)),
        ("AC_CONFIG_TESTDIR", (vec![Lit, Lit], Cmds, None)),
        // Other macros
        (
            "ifelse",
            (vec![Lit, Lit, Cmds, Cmds], Cmds, Some((0, 3))),
        ),
    ]
    .map(|(name, t)| (name.to_string(), t)));
}
