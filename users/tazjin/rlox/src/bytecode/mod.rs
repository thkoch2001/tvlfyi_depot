//! Bytecode interpreter for Lox.
//!
//! https://craftinginterpreters.com/chunks-of-bytecode.html

mod chunk;
mod errors;
mod opcode;
mod value;
mod vm;

use chunk::Chunk;
use opcode::OpCode;

pub fn main() {
    let mut chunk: Chunk = Default::default();

    let constant = chunk.add_constant(1.2);
    chunk.add_op(OpCode::OpConstant(constant), 1);
    chunk.add_op(OpCode::OpReturn, 1);

    vm::interpret(chunk).expect("it should work");
}
