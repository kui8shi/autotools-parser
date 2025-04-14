use autoconf_parser::lexer::Lexer;
use autoconf_parser::parse::MinimalParser;
use owned_chars::OwnedCharsExt;
use std::error::Error;

use std::io::{stdin, BufRead, BufReader};

fn main() -> Result<(), Box<dyn Error>> {
    let stdin = BufReader::new(stdin())
        .lines()
        .map(Result::unwrap)
        .flat_map(|mut line| {
            line.push('\n'); // BufRead::lines unfortunately strips \n and \r\n
            line.into_chars()
        });

    // Initialize our token lexer and shell parser with the program's input
    let lex = Lexer::new(stdin);
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
