extern crate bean;

use bean::context::Context;
use bean::std_scope;
use std::io::{self, Read};
use std::time::SystemTime;

fn main() {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer).unwrap();
    let before = SystemTime::now();
    let mut context = Context::new();
    context.push_scope(std_scope::scope());
    let result = context.eval(&buffer);
    let elapsed = SystemTime::now().duration_since(before).unwrap();
    println!("Total execution time: {:?}", elapsed);
    println!("{:#?}", result);
}
