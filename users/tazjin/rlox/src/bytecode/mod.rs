//! Bytecode interpreter for Lox.
//!
//! https://craftinginterpreters.com/chunks-of-bytecode.html

mod chunk;
mod compiler;
mod errors;
mod opcode;
mod value;
mod vm;

use chunk::Chunk;
pub struct Interpreter {}

impl crate::Lox for Interpreter {
    type Error = errors::Error;
    type Value = value::Value;

    fn create() -> Self {
        Interpreter {}
    }

    fn interpret(
        &mut self,
        code: String,
    ) -> Result<Self::Value, Vec<Self::Error>> {
        let chunk = compiler::compile(&code)?;
        vm::interpret(chunk).map_err(|e| vec![e])
    }
}

// pub fn main() {
//     let mut chunk: Chunk = Default::default();
//     let constant = chunk.add_constant(1.2);
//     chunk.add_op(OpCode::OpConstant(constant), 1);
//     let constant = chunk.add_constant(2.0);
//     chunk.add_op(OpCode::OpConstant(constant), 2);
//     chunk.add_op(OpCode::OpAdd, 3);
//     chunk.add_op(OpCode::OpReturn, 4);
//     vm::interpret(chunk).expect("it should work");
// }
