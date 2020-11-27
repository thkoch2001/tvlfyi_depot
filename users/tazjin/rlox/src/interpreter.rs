use crate::scanner;

// Run some Lox code and print it to stdout
pub fn run(code: &str) {
    let chars: Vec<char> = code.chars().collect();
    let _tokens = scanner::scan(&chars);
    println!("no interpreter yet, sorry")
}
