use std::ops::Index;

use super::opcode::{CodeIdx, ConstantIdx, OpCode};
use super::value;

// In the book, this type is a hand-rolled dynamic array
// implementation in C. The main benefit of following that approach
// would be avoiding issues with OpCode variants not having equal
// sizes, but for the purpose of this I'm going to ignore that
// problem.
#[derive(Debug, Default)]
pub struct Chunk {
    pub code: Vec<OpCode>,
    lines: Vec<Span>,
    constants: Vec<value::Value>,
}

#[derive(Debug)]
struct Span {
    /// Source code line
    line: usize,

    /// Number of instructions derived from this line
    count: usize,
}

impl Chunk {
    pub fn add_op(&mut self, data: OpCode, line: usize) -> CodeIdx {
        let idx = self.code.len();
        self.code.push(data);
        self.add_line(line);
        CodeIdx(idx)
    }

    pub fn add_constant(&mut self, data: value::Value) -> usize {
        let idx = self.constants.len();
        self.constants.push(data);
        idx
    }

    pub fn constant(&self, idx: ConstantIdx) -> &value::Value {
        self.constants.index(idx.0)
    }

    fn add_line(&mut self, line: usize) {
        match self.lines.last_mut() {
            Some(span) if span.line == line => span.count += 1,
            _ => self.lines.push(Span { line, count: 1 }),
        }
    }

    pub fn get_line(&self, offset: usize) -> usize {
        let mut pos = 0;
        for span in &self.lines {
            pos += span.count;
            if pos > offset {
                return span.line;
            }
        }

        panic!("invalid chunk state: line missing for offset {}", offset);
    }
}

// Disassembler

/// Print a single disassembled instruction at the specified offset.
/// Some instructions are printed "raw", others have special handling.
#[cfg(feature = "disassemble")]
pub fn disassemble_instruction(chunk: &Chunk, offset: usize) {
    print!("{:04} ", offset);

    let line = chunk.get_line(offset);
    if offset > 0 && line == chunk.get_line(offset - 1) {
        print!("   | ");
    } else {
        print!("{:4} ", line);
    }

    match chunk.code.index(offset) {
        OpCode::OpConstant(idx) => {
            println!("OpConstant({:?}) '{:?}'", idx, chunk.constant(*idx))
        }
        op => println!("{:?}", op),
    }
}

#[cfg(feature = "disassemble")]
pub fn disassemble_chunk(chunk: &Chunk) {
    for (idx, _) in chunk.code.iter().enumerate() {
        disassemble_instruction(chunk, idx);
    }
}
