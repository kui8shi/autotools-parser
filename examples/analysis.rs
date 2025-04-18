//! An example of how to use the DependencyAnalyzer to analyze variable dependencies
//! in a shell script or autoconf file.

use autoconf_parser::analyzer::DependencyAnalyzer;
use autoconf_parser::lexer::Lexer;
use autoconf_parser::parse::MinimalParser;
use owned_chars::OwnedCharsExt;
use std::{
    fmt::write,
    fs::File,
    io::{stdin, BufRead, BufReader, Write},
};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Read input from stdin
    let stdin = BufReader::new(stdin())
        .lines()
        .map(|result| result.expect("stdin error"))
        .flat_map(|mut line| {
            line.push('\n'); // BufRead::lines unfortunately strips \n and \r\n
            line.into_chars()
        });

    // Initialize the lexer and parser
    let lex = Lexer::new(stdin);
    let parser = MinimalParser::new(lex);

    // Create and run the dependency analyzer
    let mut analyzer = DependencyAnalyzer::new(parser);
    analyzer.analyze();

    // Print information about the analyzed script
    println!("Total commands: {}", analyzer.command_count());

    // Display all variables and their definitions
    let mut all_vars = std::collections::HashSet::new();
    for i in 0..analyzer.command_count() {
        if let Some(defines) = analyzer.get_defined_variables(i) {
            all_vars.extend(defines.clone());
        }
    }

    println!("\nVariable definitions:");
    for var in all_vars.iter() {
        if let Some(def_indices) = analyzer.get_definitions(var) {
            print!("  {} defined at command(s): ", var);
            for (idx, &cmd_idx) in def_indices.iter().enumerate() {
                if idx > 0 {
                    print!(", ");
                }
                print!("{}", cmd_idx);
            }
            println!();
        }
    }

    // Display dependency information for each command
    println!("\nCommand dependencies:");
    for i in 0..analyzer.command_count() {
        print!("Command {}: ", i);

        // Print the command (simplified)
        if let Some(cmd) = analyzer.get_command(i) {
            print!("{:?}", cmd);
        }
        println!();

        // Print defined variables
        if let Some(defines) = analyzer.get_defined_variables(i) {
            if !defines.is_empty() {
                print!("  Defines: ");
                for (idx, var) in defines.iter().enumerate() {
                    if idx > 0 {
                        print!(", ");
                    }
                    print!("{}", var);
                }
                println!();
            }
        }

        // Print used variables
        if let Some(uses) = analyzer.get_used_variables(i) {
            if !uses.is_empty() {
                print!("  Uses: ");
                for (idx, var) in uses.iter().enumerate() {
                    if idx > 0 {
                        print!(", ");
                    }
                    print!("{}", var);
                }
                println!();
            }
        }

        // Print dependencies
        if let Some(deps) = analyzer.get_dependencies(i) {
            if !deps.is_empty() {
                print!("  Depends on commands: ");
                for (idx, &dep) in deps.iter().enumerate() {
                    if idx > 0 {
                        print!(", ");
                    }
                    print!("{}", dep);
                }
                println!();
            }
        }

        // Print dependents
        if let Some(deps) = analyzer.get_dependents(i) {
            if !deps.is_empty() {
                print!("  Commands that depend on this: ");
                for (idx, &dep) in deps.iter().enumerate() {
                    if idx > 0 {
                        print!(", ");
                    }
                    print!("{}", dep);
                }
                println!();
            }
        }

        println!();
    }

    // Example of finding all commands related to a specific variable
    println!("\nExample variable analysis:");
    if !all_vars.is_empty() {
        let example_var = all_vars.iter().next().unwrap();
        println!("Commands related to variable '{}': ", example_var);

        let related_cmds = analyzer.find_commands_with_variable(example_var);
        for cmd_idx in related_cmds {
            println!("  Command {}: {:?}", cmd_idx, analyzer.get_command(cmd_idx));
        }

        let mut groups = Vec::new();
        let mut belongs_to = std::collections::HashMap::new();
        for cmd_idx in 0..analyzer.command_count() {
            if !belongs_to.contains_key(&cmd_idx) {
                let grp_idx = groups.len();
                let mut group = std::collections::HashSet::new();
                let mut stack = vec![cmd_idx];
                while let Some(cmd_idx) = stack.pop() {
                    if !belongs_to.contains_key(&cmd_idx) {
                        group.insert(cmd_idx);
                        belongs_to.insert(cmd_idx, grp_idx);
                        if let Some(deps) = analyzer.get_dependencies(cmd_idx) {
                            for dep in deps {
                                stack.push(*dep);
                            }
                        }
                    }
                }
                groups.push(group);
            }
        }

        let (groups, belongs_to) = (groups, belongs_to);
        for (grp_idx, group) in groups.iter().enumerate() {
            println!("Group {}: ", grp_idx);
            for cmd_idx in group {
                if let Some(cmd) = analyzer.get_command(*cmd_idx) {
                    println!("  Command {}: {:?}", cmd_idx, cmd.range);
                }
            }
        }

        // println!("  Group {}: {:?}", group_idx, analyzer.get_command(cmd_idx));
    }

    // Export graph in DOT format
    let mut dot_file = File::create("/tmp/dependencies.dot")?;
    writeln!(dot_file, "digraph Dependencies {{")?;
    // writeln!(dot_file, "  rankdir=LR;")?;

    // Node definitions
    for i in 0..analyzer.command_count() {
        let label = format!("Cmd {}", i);
        writeln!(dot_file, r#"  {} [label="{}"];"#, i, label)?;
    }

    // Edges (dependencies)
    for i in 0..analyzer.command_count() {
        if let Some(deps) = analyzer.get_dependencies(i) {
            for &dep in deps {
                writeln!(dot_file, "  {} -> {};", dep, i)?; // dep must come before i
            }
        }
    }
    writeln!(dot_file, "}}")?;
    println!("DOT graph written to dependencies.dot");

    Ok(())
}
