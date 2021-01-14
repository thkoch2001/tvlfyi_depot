use std::env;
use std::fs;
use std::io;
use std::io::Write;
use std::process;

mod errors;
mod interpreter;
mod parser;
mod scanner;

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
    let mut lox = interpreter::Interpreter::create();
    run(&mut lox, &contents);
}

// Evaluate Lox code interactively in a shitty REPL.
fn run_prompt() {
    let mut line = String::new();
    let mut lox = interpreter::Interpreter::create();

    loop {
        print!("> ");
        io::stdout().flush().unwrap();
        io::stdin()
            .read_line(&mut line)
            .expect("failed to read user input");
        run(&mut lox, &line);
        line.clear();
    }
}

fn run(lox: &mut interpreter::Interpreter, code: &str) {
    let chars: Vec<char> = code.chars().collect();

    match scanner::scan(&chars) {
        Ok(tokens) => match parser::parse(tokens) {
            Ok(program) => {
                println!("Program:\n{:?}", program);
                if let Err(err) = lox.interpret(&program) {
                    println!("Error in program: {:?}", err);
                }
            }
            Err(errors) => report_errors(errors),
        },
        Err(errors) => report_errors(errors),
    }
}

fn report_errors(errors: Vec<errors::Error>) {
    for error in errors {
        errors::report(&error);
    }
}
