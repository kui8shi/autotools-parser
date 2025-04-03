use autoconf_parser::preprocess::partial_expansion;
use std::env;
use std::fs;
use std::io::{self, Write};
use std::path::Path;

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    
    if args.len() < 2 || args.len() > 3 {
        eprintln!("Usage: {} <input_path>", args[0]);
        eprintln!("  Writes output to STDOUT");
        return Ok(());
    }
    
    let path = Path::new(&args[1]);
    
    // Process the input
    let output = partial_expansion(&path)?;
    
    // Write the output
    io::stdout().write_all(output.as_bytes())?;
    
    Ok(())
}
