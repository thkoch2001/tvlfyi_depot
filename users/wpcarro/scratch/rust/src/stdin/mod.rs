use std::io::Write;
use std::process::{Command, Stdio};

// Example of piping-in a string defined in Rust to a shell command.
pub fn example() {
    let input = "Hello, world!";

    let mut cat = Command::new("cat")
        .stdin(Stdio::piped())
        .spawn()
        .ok()
        .unwrap();

    cat.stdin
        .take()
        .unwrap()
        .write_all(&input.as_bytes())
        .unwrap();

    let output = cat.wait_with_output().unwrap();
    println!("{}", String::from_utf8_lossy(&output.stdout));
}
