//! Preprocess m4 files for partially expansion of m4 macros
mod rewrite_quotes;

use regex::{Captures, Regex};
use rewrite_quotes::QuoteRewriteConfig;
use std::collections::HashSet;
use std::env::current_dir;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};
use std::process::Command;

/// Matches to m4_include including files
const REGEX_M4_INCLUDE: &str = r"m4_include\(\[?([^\[\]\)]+)\]?\)";

/// Collect all included m4 file paths
pub fn collect_m4_include_paths(ac_path: &Path) -> io::Result<HashSet<PathBuf>> {
    let parent_dir = ac_path.parent().unwrap_or(Path::new("."));
    let mut included_paths = collect_m4_includes(ac_path)?;

    let aclocal_path = parent_dir.join("aclocal.m4");
    if aclocal_path.exists() {
        included_paths.extend(collect_m4_includes(aclocal_path.as_path())?);
    }

    let acinclude_path = parent_dir.join("acinclude.m4");
    if acinclude_path.exists() {
        included_paths.insert(acinclude_path);
    }

    Ok(included_paths)
}

/// Collects m4 include directives from the given file path
///
/// This function scans a file for m4_include directives and returns the paths
/// of included files. The included file paths are assumed to be relative to
/// the parent directory of the input file.
///
/// # Arguments
///
/// * `path` - Path to the m4/autoconf file to scan
///
/// # Returns
///
/// A vector of paths to included files
pub fn collect_m4_includes(path: &Path) -> io::Result<HashSet<PathBuf>> {
    let content = fs::read_to_string(path)?;
    let parent_dir = path.parent().unwrap_or(Path::new("."));

    // Regular expression to match m4_include directives
    let include_regex = Regex::new(REGEX_M4_INCLUDE).unwrap();

    let mut included_paths = HashSet::new();

    for caps in include_regex.captures_iter(&content) {
        if let Some(included_file) = caps.get(1) {
            let included_path = parent_dir.join(included_file.as_str());
            if included_path.exists() {
                included_paths.insert(included_path);
            }
        }
    }

    Ok(included_paths)
}

/// Expand m4 include directives from the given file path
///
/// This function scans a file for m4_include directives and return the
/// file content while expanding the directives according to the following rules:
/// 1. if the included file paths have m4 extension, remove the directives.
/// 2. otherwise, expand the directives with the corresponding file contents.
///
/// The included file paths are assumed to be relative to the parent directory of the input file.
///
/// # Arguments
///
/// * `ac_path` - Path to the m4/autoconf file to scan
///
/// # Returns
///
/// A string of the file content with m4 include directives expanded
pub fn expand_m4_includes(ac_path: &Path) -> io::Result<String> {
    let content = fs::read_to_string(ac_path)?;
    let parent_dir = ac_path.parent().unwrap_or(Path::new("."));

    // Regular expression to match m4_include directives
    let include_regex = Regex::new(REGEX_M4_INCLUDE).unwrap();

    let content = include_regex
        .replace_all(&content, |caps: &Captures<'_>| {
            if let Some(included_file) = caps.get(1) {
                let included_path = parent_dir.join(included_file.as_str());
                let is_m4 = included_path
                    .extension()
                    .is_some_and(|ext| ext.to_str() == Some("m4"));
                if included_path.exists() && !is_m4 {
                    if let Ok(included_content) = fs::read_to_string(&included_path) {
                        return included_content.trim().to_string();
                    }
                }
            }
            "".into()
        })
        .to_string();
    Ok(content)
}

/// Performs partial expansion of m4 macros in the given file
///
/// This function processes an m4/autoconf file by:
/// 1. Collecting all included files
/// 2. Removing m4_include directives
/// 3. Rewriting quotes in the main file and included files
/// 4. Concatenating all files together
/// 5. Adding a header for m4 diversion and quote handling
/// 6. Running autom4te with m4sugar language on the resulting file
///
/// # Arguments
///
/// * `ac_path` - Path to the m4/autoconf file to process
///
/// # Returns
///
/// The stdout from the autom4te command or an error
pub fn partial_expansion(ac_path: &Path) -> io::Result<String> {
    let project_dir = ac_path
        .parent()
        .map(|path| path.to_path_buf())
        .unwrap_or(current_dir().unwrap());

    // Create a temporary file for the processed content
    let ac_tmp_path = std::env::temp_dir().join("autoconf_processed.m4");

    // Collect included files
    let include_paths = collect_m4_include_paths(ac_path)?;
    let ignore_patterns = Vec::from([
        "ax_",
        "pkg.m4",
        "libtool.m4",
        "ltoptions.m4",
        "ltsugar.m4",
        "ltversion.m4",
        "lt~obsolete.m4",
    ]);
    let filtered_include_paths = include_paths.into_iter().filter(|path| {
        let filename = path.file_name().unwrap().to_owned().into_string().unwrap();
        ignore_patterns.iter().all(|pat| !filename.contains(pat))
    });

    let mut processed_content = String::new();

    fn process(label: &str, content: &str, main_content: Option<&str>) -> String {
        let mut out = format!("\ndnl ==== {} ====\n", label);
        out.push_str(&&rewrite_quotes::rewrite_quotes_with_config(
            content,
            QuoteRewriteConfig {
                main_content: main_content.map(|s| s.to_owned()),
                ..Default::default()
            },
        ));
        out
    }

    // Read the main file content while expanding m4_include directives
    let main_content = expand_m4_includes(ac_path)?;

    // Process and append content from included files
    for path in filtered_include_paths {
        let filename = path.file_name().unwrap().to_owned().into_string().unwrap();
        let is_m4 = path
            .extension()
            .is_some_and(|ext| ext.to_str() == Some("m4"));
        if path.exists() && is_m4 {
            if let Ok(include_content) = fs::read_to_string(path) {
                // Process quotes in the included file
                let processed_include = process(&filename, &include_content, Some(&main_content));
                processed_content.push_str(&processed_include);
            }
        }
    }

    // Process quotes in the main file
    processed_content.push_str(&process("configure.ac", &main_content, None));

    // Prepare the header content
    let header = "m4_divert(0)\n\
                 m4_define([m4_divert_text], [$2])\n\
                 m4_define([m4_version_prereq], [$2])\n\
                 m4_define([m4_foreach], [$1])\n\
                 m4_define([AC_TRY_EVAL], [$$1])\n\
                 m4_define([AC_TRY_COMMAND], [$1])\n\
                 m4_define([AS_VAR_GET], [${$1}])\n\
                 m4_undefine([m4_pushdef])\n\
                 m4_undefine([m4_popdef])\n\
                 m4_undefine([m4_normalize])\n\
                 m4_changequote(|OPENQUOTE|,|CLOSEQUOTE|)\n\n";

    // Prepend the header to the processed content
    let final_content = format!("{}{}", header, processed_content);

    // Write the final content to the temporary file
    fs::write(&ac_tmp_path, final_content)?;

    // Run autom4te on the temporary file
    let output = Command::new("autom4te")
        .arg("--language=m4sugar")
        .arg(ac_tmp_path)
        .current_dir(project_dir)
        .output()?;

    if !output.status.success() {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            format!(
                "autom4te failed: {}",
                String::from_utf8_lossy(&output.stderr)
            ),
        ));
    }

    let result = String::from_utf8_lossy(&output.stdout)
        .trim_start()
        .to_owned();

    // Return the stdout from autom4te
    Ok(result)
}
