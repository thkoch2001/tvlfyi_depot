use std::env;
use std::fs;
use std::io;
use std::io::Write;
use std::process;

mod bytecode;
mod treewalk;

fn main() {
    match env::var("LOX_INTERPRETER").as_ref().map(String::as_str) {
        Ok("treewalk") => treewalk::main(),
        _ => bytecode::main(),
    }
}
