use crate::errors::{report, Error};
use crate::parser;
use crate::scanner::{self, Token};

// Run some Lox code and print it to stdout
pub fn run(code: &str) {
    let chars: Vec<char> = code.chars().collect();

    match scanner::scan(&chars) {
        Ok(tokens) => {
            print_tokens(&tokens);
            match parser::parse(tokens) {
                Ok(expr) => println!("Expression:\n{:?}", expr),
                Err(errors) => report_errors(errors),
            }
        }
        Err(errors) => report_errors(errors),
    }
}

fn print_tokens<'a>(tokens: &Vec<Token<'a>>) {
    println!("Tokens:");
    for token in tokens {
        println!("{:?}", token);
    }
}

fn report_errors(errors: Vec<Error>) {
    for error in errors {
        report(&error);
    }
}
