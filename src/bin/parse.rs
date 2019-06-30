extern crate bean;

use bean::parser::Parser;
use std::io::{self, Read};

fn main() {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer).unwrap();
    println!("{:#?}", Parser::parse_script(&buffer));
}
