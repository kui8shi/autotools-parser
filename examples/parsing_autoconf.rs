use autotools_parser::lexer::Lexer;
use autotools_parser::parse::autoconf::NodeParser;
use std::error::Error;

use std::io::{stdin, Read};

fn main() -> Result<(), Box<dyn Error>> {
    let mut input = String::new();
    stdin().read_to_string(&mut input)?;
    // Initialize our token lexer and shell parser with the program's input
    let lex = Lexer::new(input.chars());
    let (nodes, top_ids) = NodeParser::<_, ()>::new(lex).parse_all();

    for id in top_ids {
        println!("============================");
        println!("{:?}", nodes.get(id));
    }

    Ok(())
}
