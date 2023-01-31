use std::io::Write;
use std::ops::{Index, IndexMut};

use crate::opcode::{CodeIdx, ConstantIdx, OpCode};
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
    data: u8,
}

impl Debug for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        // SAFETY: Printing an op's u8 representation is always safe, but the
        // actual value is of course nonsense in some cases. We can do some
        // best-effort guessing in the future.
        unsafe { write!(f, "Op {{ data: {} }}", self.data) }
    }
}

/// A chunk is a representation of a sequence of bytecode instructions,
/// associated constants and additional metadata as emitted by the compiler.
#[derive(Debug, Default)]
pub struct Chunk {
    pub code: Vec<Op>,
    pub constants: Vec<Value>,
    spans: Vec<SourceSpan>,
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
        let idx = self.code.len();
        self.code.push(Op { op });
        self.push_span(span);
        CodeIdx(idx)
    }

    pub fn push_usize(&mut self, input: usize, span: codemap::Span) {
        for data in input.to_le_bytes() {
            self.code.push(Op { data });
            self.push_span(span);
        }
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

    /// Reads a single usize operand at the specified code index.
    pub fn read_usize_operand(&self, idx: CodeIdx) -> usize {
        // SAFETY: It is always safe to read u8 representations of the
        // operations, but for an invalid index the returned value will of
        // course be nonsense. At the end of a chunk, Rust would panic for the
        // out-of-bounds index access.
        unsafe {
            usize::from_le_bytes([
                self.code[idx.0].data,
                self.code[idx.0 + 1].data,
                self.code[idx.0 + 2].data,
                self.code[idx.0 + 3].data,
                self.code[idx.0 + 4].data,
                self.code[idx.0 + 5].data,
                self.code[idx.0 + 6].data,
                self.code[idx.0 + 7].data,
            ])
        }
    }

    /// Patch the jump instruction at the given index, setting its jump offset
    /// from the placeholder to the position of the *next* instruction to be
    /// emitted.
    ///
    /// This is required because the actual target offset of jumps is
    /// not known at the time when the jump operation itself is
    /// emitted.
    pub(crate) fn patch_jump(&mut self, idx: CodeIdx) {
        // SAFETY: This function is only used in the compiler, and only on known
        // valid CodeIdx instances. Using it in other contexts is unsafe.
        unsafe {
            match &mut self.code[idx.0] {
                Op {
                    op:
                        OpCode::OpJump
                        | OpCode::OpJumpIfFalse
                        | OpCode::OpJumpIfTrue
                        | OpCode::OpJumpIfNotFound,
                } => {
                    // Calculate offset between the place where the jump was
                    // inserted, and the next op. The `8` accounts for the
                    // jump's operand itself, as the instruction pointer will be
                    // behind that when the jump is executed.
                    let offset = self.code.len() - 1 - idx.0 - 8;

                    for (n, data) in offset.to_le_bytes().into_iter().enumerate() {
                        self.code[idx.0 + 1 + n] = Op { data };
                    }
                }

                op => panic!("attempted to patch unsupported op: {:?}", op),
            }
        }
    }

    /// Perform tail-call optimisation if the last call within a
    /// compiled chunk is another call.
    // TODO(tazjin): this might have potential to produce very hard-to-debug
    // cases with the optimised opcode representation, try to trigger it.
    pub(crate) fn optimise_tail_call(&mut self) {
        let last_op = self
            .code
            .last_mut()
            .expect("compiler bug: chunk should never be empty");

        // SAFETY: This is unsafe, and likely dangerous. It could mutate an
        // operand to an op by accident, if that operand matches the numerical
        // value of `OpCall`. This is potentially a BUG as soon as operands are
        // serialised into the flat structure, but we will detect and fix it
        // quickly.
        unsafe {
            if matches!(last_op, Op { op: OpCode::OpCall }) {
                *last_op = Op {
                    op: OpCode::OpTailCall,
                };
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
    ///
    /// If the operation at `idx` has operands, the index pointer will be
    /// incremented to point *to the last byte of the last operand*.
    pub fn disassemble_op<W: Write>(
        &self,
        writer: &mut W,
        idx: &mut usize,
        source: &SourceCode,
        width: usize,
    ) -> Result<(), std::io::Error> {
        write!(writer, "{:#width$x}\t ", *idx, width = width)?;

        // Print continuation character if the previous operation was at
        // the same line, otherwise print the line.
        let line = source.get_line(self.get_span(CodeIdx(*idx)));
        if *idx > 0 && source.get_line(self.get_span(CodeIdx(*idx - 1))) == line {
            write!(writer, "   |\t")?;
        } else {
            write!(writer, "{:4}\t", line)?;
        }

        // Operations with operands need to be handled here.
        match self[CodeIdx(*idx)] {
            // Special snowflakes
            OpCode::OpConstant => {
                let constant_idx = ConstantIdx(self.read_usize_operand(CodeIdx(*idx + 1)));
                *idx += 8;

                writeln!(
                    writer,
                    "OpConstant({}@{})",
                    self[constant_idx], constant_idx.0
                )
            }

            // Operations with a single usize operand
            op @ OpCode::OpJump
            | op @ OpCode::OpJumpIfFalse
            | op @ OpCode::OpJumpIfTrue
            | op @ OpCode::OpJumpIfNotFound
            | op @ OpCode::OpAttrs
            | op @ OpCode::OpPushWith
            | op @ OpCode::OpList
            | op @ OpCode::OpInterpolate => {
                let operand = self.read_usize_operand(CodeIdx(*idx + 1));
                *idx += 8;

                writeln!(writer, "{:?}({})", op, operand)
            }

            op => writeln!(writer, "{:?}", op),
        }?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::dummy_span;

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

    #[test]
    fn patch_jump() {
        let span = dummy_span();
        let mut chunk = Chunk::default();
        let idx = chunk.push_op(OpCode::OpJump, span);
        chunk.push_usize(852935729, span);
        chunk.push_op(OpCode::OpAdd, span);
        chunk.patch_jump(idx);

        unsafe {
            assert_eq!(chunk.code[0].op, OpCode::OpJump);
            // jump should jump over 1 operation (OpAdd), to end up on the
            // *next* one
            assert_eq!(chunk.read_usize_operand(CodeIdx(1)), 1);
            assert_eq!(chunk.code[9].op, OpCode::OpAdd);
        }
    }
}
