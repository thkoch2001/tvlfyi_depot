use std::process;
use std::env;

fn run_file(_file: &str) {
    unimplemented!("no file support yet")
}

fn run_prompt() {
    unimplemented!("no prompt support yet")
}

fn main() {
    let mut args = env::args();

    if args.len() > 1 {
        println!("Usage: rlox [script]");
        process::exit(1);
    } else if let Some(file) = args.next() {
        run_file(&file);
    } else {
        run_prompt();
    }
}
