use autoconf_parser::lexer::Lexer;
use autoconf_parser::autoconf::MinimalParser;
use std::error::Error;

use std::io::{stdin, Read};

fn main() -> Result<(), Box<dyn Error>> {
    let mut input = String::new();
    stdin().read_to_string(&mut input)?;
    // Initialize our token lexer and shell parser with the program's input
    let lex = Lexer::new(input.chars());
    let parser = MinimalParser::new(lex);

    // Parse our input!
    for res in parser {
        match res {
            Ok(cmd) => {
                dbg!(cmd);
            }
            Err(e) => {
                println!("{}", e);
                panic!();
            }
        }
    }
    Ok(())
}
