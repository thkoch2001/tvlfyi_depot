use std::ops::Index;

use super::opcode::OpCode;
use super::value;

// In the book, this type is a hand-rolled dynamic array
// implementation in C. The main benefit of following that approach
// would be avoiding issues with OpCode variants not having equal
// sizes, but for the purpose of this I'm going to ignore that
// problem.
#[derive(Debug, Default)]
pub struct Chunk {
    code: Vec<OpCode>,
    constants: Vec<value::Value>,
}

impl Chunk {
    pub fn add_op(&mut self, data: OpCode) -> usize {
        let idx = self.code.len();
        self.code.push(data);
        idx
    }

    pub fn add_constant(&mut self, data: value::Value) -> usize {
        let idx = self.constants.len();
        self.constants.push(data);
        idx
    }
}

impl Index<usize> for Chunk {
    type Output = OpCode;

    fn index(&self, offset: usize) -> &Self::Output {
        self.code.index(offset)
    }
}

// Disassembler
pub fn disassemble(chunk: &Chunk, name: &str) {
    println!("== {} ==", name);

    for (idx, _) in chunk.code.iter().enumerate() {
        disassemble_instruction(chunk, idx);
    }
}

/// Print a single disassembled instruction at the specified offset.
/// Some instructions are printed "raw", others have special handling.
fn disassemble_instruction(chunk: &Chunk, offset: usize) {
    print!("{:04} ", offset);

    match &chunk[offset] {
        OpCode::OpConstant(idx) => println!("OpConstant idx '{:?}'", chunk.constants[*idx]),
        op => println!("{:?}", op),
    }
}
