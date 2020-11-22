use std::env;
use std::fs;
use std::io;
use std::io::Write;
use std::process;

mod interpreter;

fn main() {
    let mut args = env::args();

    if args.len() > 2 {
        println!("Usage: rlox [script]");
        process::exit(1);
    } else if let Some(file) = args.nth(1) {
        run_file(&file);
    } else {
        run_prompt();
    }
}

// Run Lox code from a file and print results to stdout
fn run_file(file: &str) {
    let contents = fs::read_to_string(file).expect("failed to read the input file");
    interpreter::run(&contents);
}

// Evaluate Lox code interactively in a shitty REPL.
fn run_prompt() {
    let mut line = String::new();

    loop {
        print!("> ");
        io::stdout().flush().unwrap();
        io::stdin()
            .read_line(&mut line)
            .expect("failed to read user input");
        interpreter::run(&line);
    }
}
