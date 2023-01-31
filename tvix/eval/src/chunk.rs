use std::io::Write;
use std::ops::{Index, IndexMut};

use crate::opcode::{CodeIdx, ConstantIdx, JumpOffset, OpCode};
use crate::value::Value;
use crate::SourceCode;
use std::fmt::Debug;

/// Represents a source location from which one or more operations
/// were compiled.
///
/// The span itself is an index into a [codemap::Codemap], and the
/// structure tracks the number of operations that were yielded from
/// the same span.
///
/// At error reporting time, it becomes possible to either just fetch
/// the textual representation of that span from the codemap, or to
/// even re-parse the AST using rnix to create more semantically
/// interesting errors.
#[derive(Clone, Debug, PartialEq)]
struct SourceSpan {
    /// Span into the [codemap::Codemap].
    span: codemap::Span,

    /// Number of instructions derived from this span.
    count: usize,
}

/// An op is a single element of the code chunk inside of some Tvix bytecode. It
/// can be either an instruction to the runtime, or an operand to an instruction.
///
/// To optimise caching behaviour of chunks of bytecode, this is represented by
/// a union type which is accessed through helpers on the chunk type.
pub union Op {
    op: OpCode,
    // TODO: data: u8,
}

impl Debug for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        // TODO: we can not safely print an arbitrary op, but we can do some
        // best-effort guessing in the future
        write!(f, "[tvix_eval::Op]")
    }
}

/// A chunk is a representation of a sequence of bytecode instructions,
/// associated constants and additional metadata as emitted by the compiler.
#[derive(Debug, Default)]
pub struct Chunk {
    pub code: Vec<Op>,
    pub constants: Vec<Value>,
    spans: Vec<SourceSpan>,
    last_op_idx: usize,
}

impl Index<ConstantIdx> for Chunk {
    type Output = Value;

    fn index(&self, index: ConstantIdx) -> &Self::Output {
        &self.constants[index.0]
    }
}

impl Index<CodeIdx> for Chunk {
    type Output = OpCode;

    fn index(&self, index: CodeIdx) -> &Self::Output {
        // SAFETY: with a few exceptions, a `CodeIdx` is always created by
        // pushing an op, so it is reasonably safe to assume that this will
        // work, but it is of course not actually statically checked.
        unsafe { &self.code[index.0].op }
    }
}

impl IndexMut<CodeIdx> for Chunk {
    fn index_mut(&mut self, index: CodeIdx) -> &mut Self::Output {
        // SAFETY: same as `Index<CodeIdx>` above.
        unsafe { &mut self.code[index.0].op }
    }
}

impl Chunk {
    pub fn push_op(&mut self, op: OpCode, span: codemap::Span) -> CodeIdx {
        self.last_op_idx = self.code.len();
        self.code.push(Op { op });
        self.push_span(span);
        CodeIdx(self.last_op_idx)
    }

    /// Pop the last operation from the chunk and clean up its tracked
    /// span. Used when the compiler backtracks.
    pub fn pop_op(&mut self) {
        // Simply drop the last op.
        self.code.pop();

        // If the last span only had this op, drop it, otherwise
        // decrease its operation counter.
        match self.spans.last_mut() {
            // If the last span had more than one op, decrease the
            // counter.
            Some(span) if span.count > 1 => span.count -= 1,

            // Otherwise, drop it.
            Some(_) => {
                self.spans.pop();
            }

            None => unreachable!(),
        }
    }

    pub fn push_constant(&mut self, data: Value) -> ConstantIdx {
        let idx = self.constants.len();
        self.constants.push(data);
        ConstantIdx(idx)
    }

    /// Patch the jump instruction at the given index, setting its
    /// jump offset from the placeholder to the current code position.
    ///
    /// This is required because the actual target offset of jumps is
    /// not known at the time when the jump operation itself is
    /// emitted.
    pub(crate) fn patch_jump(&mut self, idx: CodeIdx) {
        let offset = JumpOffset(self.code.len() - 1 - idx.0);

        // SAFETY: This function is only used in the compiler, and only on known
        // valid CodeIdx instances. Using it in other contexts is unsafe.
        unsafe {
            match &mut self.code[idx.0] {
                Op {
                    op: OpCode::OpJump(n),
                }
                | Op {
                    op: OpCode::OpJumpIfFalse(n),
                }
                | Op {
                    op: OpCode::OpJumpIfTrue(n),
                }
                | Op {
                    op: OpCode::OpJumpIfNotFound(n),
                } => {
                    *n = offset;
                }

                op => panic!("attempted to patch unsupported op: {:?}", op),
            }
        }
    }

    /// Retrieve a copy of the last operation in the chunk.
    pub(crate) fn last_op(&self) -> Option<OpCode> {
        if self.last_op_idx == 0 {
            return None;
        }

        // SAFETY: This is safe because last_op_idx is only modified
        // when pushing an operation inside of this type, so we always
        // know that this index points towards an op and not some data.
        unsafe { Some(self.code[self.last_op_idx].op) }
    }

    /// Perform tail-call optimisation if the last call within a
    /// compiled chunk is another call.
    pub(crate) fn optimise_tail_call(&mut self) {
        if let Some(OpCode::OpCall) = self.last_op() {
            self.code[self.last_op_idx] = Op {
                op: OpCode::OpTailCall,
            }
        }
    }

    // Span tracking implementation

    fn push_span(&mut self, span: codemap::Span) {
        match self.spans.last_mut() {
            // We do not need to insert the same span again, as this
            // instruction was compiled from the same span as the last
            // one.
            Some(last) if last.span == span => last.count += 1,

            // In all other cases, this is a new source span.
            _ => self.spans.push(SourceSpan { span, count: 1 }),
        }
    }

    /// Retrieve the [codemap::Span] from which the instruction at
    /// `offset` was compiled.
    pub fn get_span(&self, offset: CodeIdx) -> codemap::Span {
        let mut pos = 0;

        for span in &self.spans {
            pos += span.count;
            if pos > offset.0 {
                return span.span;
            }
        }

        panic!("compiler error: chunk missing span for offset {}", offset.0);
    }

    /// Write the disassembler representation of the operation at
    /// `idx` to the specified writer.
    pub fn disassemble_op<W: Write>(
        &self,
        writer: &mut W,
        source: &SourceCode,
        width: usize,
        idx: CodeIdx,
    ) -> Result<(), std::io::Error> {
        write!(writer, "{:#width$x}\t ", idx.0, width = width)?;

        // Print continuation character if the previous operation was at
        // the same line, otherwise print the line.
        let line = source.get_line(self.get_span(idx));
        if idx.0 > 0 && source.get_line(self.get_span(CodeIdx(idx.0 - 1))) == line {
            write!(writer, "   |\t")?;
        } else {
            write!(writer, "{:4}\t", line)?;
        }

        match self[idx] {
            OpCode::OpConstant(idx) => writeln!(writer, "OpConstant({}@{})", self[idx], idx.0),
            op => writeln!(writer, "{:?}", op),
        }?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::test_utils::dummy_span;

    use super::*;
    #[test]
    fn push_op() {
        let mut chunk = Chunk::default();
        chunk.push_op(OpCode::OpAdd, dummy_span());

        unsafe {
            assert!(matches!(
                chunk.code.last().unwrap(),
                Op { op: OpCode::OpAdd }
            ));
        }
    }
}
