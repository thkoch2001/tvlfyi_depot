use crate::*;

pub mod interpreter;
mod resolver;

pub fn main() {
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
    let mut lox = treewalk::interpreter::Interpreter::create();
    run(&mut lox, &contents);
}

// Evaluate Lox code interactively in a shitty REPL.
fn run_prompt() {
    let mut line = String::new();
    let mut lox = treewalk::interpreter::Interpreter::create();

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

fn run(lox: &mut treewalk::interpreter::Interpreter, code: &str) {
    let chars: Vec<char> = code.chars().collect();

    let result = scanner::scan(&chars)
        .and_then(|tokens| parser::parse(tokens))
        .and_then(|program| lox.interpret(program).map_err(|e| vec![e]));

    if let Err(errors) = result {
        report_errors(errors);
    }
}
