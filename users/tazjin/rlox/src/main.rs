use std::env;
use std::fs;
use std::io;
use std::io::Write;
use std::process;

mod bytecode;
mod scanner;
mod treewalk;

/// Trait for making the different interpreters callable in the same
/// way.
pub trait Lox {
    type Value: std::fmt::Debug;
    type Error: std::fmt::Display;

    fn create() -> Self;
    fn interpret(
        &mut self,
        source: String,
    ) -> Result<Self::Value, Vec<Self::Error>>;
}

fn main() {
    let mut args = env::args();
    if args.len() > 2 {
        println!("Usage: rlox [script]");
        process::exit(1);
    }

    match env::var("LOX_INTERPRETER").as_ref().map(String::as_str) {
        Ok("treewalk") => {
            pick::<treewalk::interpreter::Interpreter>(args.nth(1))
        }
        _ => pick::<bytecode::Interpreter>(args.nth(1)),
    }
}

fn pick<I: Lox>(file_arg: Option<String>) {
    if let Some(file) = file_arg {
        run_file::<I>(&file);
    } else {
        run_prompt::<I>();
    }
}

// Run Lox code from a file and print results to stdout
fn run_file<I: Lox>(file: &str) {
    let contents =
        fs::read_to_string(file).expect("failed to read the input file");
    let mut lox = I::create();
    run(&mut lox, contents);
}

// Evaluate Lox code interactively in a shitty REPL.
fn run_prompt<I: Lox>() {
    let mut line = String::new();
    let mut lox = I::create();

    loop {
        print!("> ");
        io::stdout().flush().unwrap();
        io::stdin()
            .read_line(&mut line)
            .expect("failed to read user input");
        run(&mut lox, std::mem::take(&mut line));
        line.clear();
    }
}

fn run<I: Lox>(lox: &mut I, code: String) {
    match lox.interpret(code) {
        Ok(result) => println!("=> {:?}", result),
        Err(errors) => {
            for error in errors {
                eprintln!("{}", error);
            }
        }
    }
}
