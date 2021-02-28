//! Bytecode interpreter for Lox.
//!
//! https://craftinginterpreters.com/chunks-of-bytecode.html

mod chunk;
mod compiler;
mod errors;
mod opcode;
mod value;
mod vm;

#[cfg(test)]
mod tests;

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
