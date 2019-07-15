extern crate ansi_term;
extern crate bean;
extern crate rustyline;

use ansi_term::Colour::{Green, Fixed, Red};
use bean::context::Context;
use bean::std_scope;
use bean::value::Object;
use rustyline::{Editor, error::ReadlineError};
use std::time::SystemTime;

fn main() {
    println!("Type \\h for help.\n");

    let mut rl = Editor::<()>::new();
    let mut line_offset = 0;
    let mut scope = Some(Object::new());
    let mut context = Context::new();
    context.push_scope(std_scope::scope());

    loop {
        match rl.readline(&format!("bean:{:04}> ", line_offset + 1)) {
            Ok(ref line) if line.starts_with("\\") => {
                let code = &line[1..line.len()];
                match code {
                    "h" => print_help(),
                    "q" => break,
                    _ => println!("Unknown command. Type \\h for help."),
                }

                rl.add_history_entry(line);
            },
            Ok(ref line) => {
                let before = SystemTime::now();
                let (changed_scope, result) = context.eval_with_scope(line, scope.take().unwrap());
                scope.replace(changed_scope);
                let elapsed = SystemTime::now().duration_since(before).unwrap();
                println!("{}", Fixed(240).paint(format!("Evaluated in {:?}", elapsed)));
                match result {
                    Ok(value) => println!("{} {:?}", Green.paint("=>"), value),
                    Err(error) => println!("{} {:?}", Red.paint("!>"), error),
                }

                let line_count = line.lines().count();
                line_offset += if line_count == 0 { 1 } else { line_count };

                rl.add_history_entry(line);
            },
            Err(ReadlineError::Interrupted) => continue,
            Err(ReadlineError::Eof) => break,
            Err(err) => {
                println!("CLI error: {:?}", err);
                break;
            },
        }
    }
}

fn print_help() {
    println!("You are using a Bean REPL.");
    println!("Type: \\q to quit");
    println!("      \\h for this dialog");
}
