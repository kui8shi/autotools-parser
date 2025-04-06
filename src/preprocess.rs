//! Preprocess m4 files for partially expansion of m4 macros
mod rewrite_quotes;

use regex::Regex;
use std::collections::HashSet;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};
use std::process::Command;

/// If m4_include is used other than including .m4 files, the whole process won't work.
const REGEX_M4_INCLUDE: &str = r"m4_include\(\[?([^\[\]\)]+)\]?\)";

/// Collects m4 include directives from the given file path
///
/// This function scans a file for m4_include directives and returns the paths
/// of included files. The included file paths are assumed to be relative to
/// the parent directory of the input file.
///
/// # Arguments
///
/// * `ac_path` - Path to the m4/autoconf file to scan
///
/// # Returns
///
/// A vector of paths to included files
pub fn collect_m4_includes(ac_path: &Path) -> io::Result<Vec<PathBuf>> {
    let content = fs::read_to_string(ac_path)?;
    let parent_dir = ac_path.parent().unwrap_or(Path::new("."));

    // Regular expression to match m4_include directives
    let include_regex = Regex::new(REGEX_M4_INCLUDE).unwrap();

    let mut included_paths = vec![parent_dir.join("acinclude.m4")];

    for cap in include_regex.captures_iter(&content) {
        if let Some(included_file) = cap.get(1) {
            let included_path = parent_dir.join(included_file.as_str());
            if included_path.exists() {
                included_paths.push(included_path);
            }
        }
    }

    Ok(included_paths)
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
    // Create a temporary file for the processed content
    let ac_tmp_path = std::env::temp_dir().join("autoconf_processed.m4");

    // Collect included files
    let included_paths = collect_m4_includes(ac_path)?;
    let excluded_paths = HashSet::from([
        "ax_cxx_compile_stdcxx.m4".to_string(), // FIXME: ignore all m4 files definining AX_* macros
        "libtool.m4".to_string(),
        "ltoptions.m4".to_string(),
        "ltsugar.m4".to_string(),
        "ltversion.m4".to_string(),
        "lt~obsolete.m4".to_string(),
    ]);

    // Read the main file content
    let main_content = fs::read_to_string(ac_path)?;

    // Remove m4_include directives
    let include_regex = Regex::new(REGEX_M4_INCLUDE).unwrap();
    let main_content_no_includes = include_regex.replace_all(&main_content, "").to_string();

    let mut processed_content = String::new();

    fn process(label: &str, content: &str) -> String {
        let mut out = format!("\ndnl ==== {} ====\n", label);
        out.push_str(&rewrite_quotes::rewrite_quotes(content));
        out
    }

    // Process and append content from included files
    for path in &included_paths {
        let filename = path.file_name().unwrap().to_owned().into_string().unwrap();
        if !excluded_paths.contains(&filename) && path.exists() {
            if let Ok(include_content) = fs::read_to_string(path) {
                // Process quotes in the included file
                let processed_include = process(&filename, &include_content);
                processed_content.push_str(&processed_include);
            }
        }
    }

    // Process quotes in the main file
    processed_content.push_str(&process("configure.ac", &main_content_no_includes));

    // Prepare the header content
    let header = "m4_divert(0)\n\
                 m4_define([m4_divert_text], [$2])\n\
                 m4_define([m4_version_prereq], [$2])\n\
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

    // Return the stdout from autom4te
    Ok(String::from_utf8_lossy(&output.stdout).to_string())
}
