use autoconf_parser::lexer::Lexer;
use autoconf_parser::parse::automake::AutomakeNodeParser;
use std::error::Error;

use std::io::{stdin, Read};

fn main() -> Result<(), Box<dyn Error>> {
    let mut input = String::new();
    stdin().read_to_string(&mut input)?;
    // Initialize our token lexer and shell parser with the program's input
    let lex = Lexer::new(input.chars());
    let parser = AutomakeNodeParser::new(lex);

    let (nodes, top_ids) = parser.parse_all();

    // Parse our input!
    for node_id in top_ids {
        dbg!(node_id, &nodes[node_id]);
    }
    Ok(())
}
