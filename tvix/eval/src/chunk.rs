use std::io::Write;
use std::mem::size_of;
use std::ops::Index;

use crate::opcode::{self, CodeIdx, ConstantIdx, JumpOffset, OpCode};
use crate::packed_encode::PackedEncode;
use crate::value::Value;
use crate::SourceCode;

/// Represents a source location from which one or more operations
/// were compiled.
///
/// The span itself is an index into a [codemap::CodeMap], and the
/// structure tracks the number of operations that were yielded from
/// the same span.
///
/// At error reporting time, it becomes possible to either just fetch
/// the textual representation of that span from the codemap, or to
/// even re-parse the AST using rnix to create more semantically
/// interesting errors.
#[derive(Clone, Debug, PartialEq)]
struct SourceSpan {
    /// Span into the [codemap::CodeMap].
    span: codemap::Span,

    /// Index of the first operation covered by this span.
    start: usize,
}

/// A chunk is a representation of a sequence of bytecode
/// instructions, associated constants and additional metadata as
/// emitted by the compiler.
#[derive(Debug, Default)]
pub struct Chunk {
    code: Vec<u8>,
    last_op_size: Option<usize>,
    pub constants: Vec<Value>,
    spans: Vec<SourceSpan>,
}

impl Index<ConstantIdx> for Chunk {
    type Output = Value;

    fn index(&self, index: ConstantIdx) -> &Self::Output {
        &self.constants[index.0]
    }
}

/*
impl Index<CodeIdx> for Chunk {
    type Output = OpCode;

    fn index(&self, index: CodeIdx) -> &Self::Output {
        &self.code[index.0]
    }
}

impl IndexMut<CodeIdx> for Chunk {
    fn index_mut(&mut self, index: CodeIdx) -> &mut Self::Output {
        &mut self.code[index.0]
    }
}
*/

impl Chunk {
    pub fn push_op(&mut self, data: OpCode, span: codemap::Span) -> CodeIdx {
        let idx = self.code.len();
        data.push(&mut self.code);
        self.last_op_size = Some(self.code.len() - idx);
        self.push_span(span, idx);
        CodeIdx(idx)
    }

    pub unsafe fn change_op(&mut self, idx: CodeIdx, new_op: OpCode) {
        self.code[idx.0] = new_op.discriminant();
    }

    /// Patch the jump instruction at the given index, setting its
    /// jump offset from the placeholder to the current code position.
    ///
    /// This is required because the actual target offset of jumps is
    /// not known at the time when the jump operation itself is
    /// emitted.
    pub unsafe fn patch_jump(&mut self, idx: CodeIdx) {
        let offset = JumpOffset(dbg!(self.last_op_idx()).unwrap().0 - dbg!(idx).0);

        if cfg!(debug_assertions) {
            let op = unsafe { OpCode::read(&self.code[idx.0..]) }.0;
            debug_assert!(
                matches!(
                    op,
                    OpCode::OpJump(_)
                        | OpCode::OpJumpIfFalse(_)
                        | OpCode::OpJumpIfTrue(_)
                        | OpCode::OpJumpIfCatchable(_)
                        | OpCode::OpJumpIfNotFound(_)
                        | OpCode::OpJumpIfNoFinaliseRequest(_)
                ),
                "attempted to patch unsupported op: {:?}",
                op
            );
        }

        self.code[(idx.0 + 1)..(idx.0 + 1 + size_of::<usize>())]
            .copy_from_slice(&offset.0.to_ne_bytes());
    }

    /// Get the first span of a chunk, no questions asked.
    pub fn first_span(&self) -> codemap::Span {
        self.spans[0].span
    }

    /// Return the last op in the chunk, if any
    pub fn last_op(&self) -> Option<OpCode> {
        self.last_op_size.map(|off| unsafe {
            let (op, size) = OpCode::read(&self.code[(self.code.len() - off)..]);
            debug_assert_eq!(size, off);
            op
        })
    }

    pub fn last_op_idx(&self) -> Option<CodeIdx> {
        Some(CodeIdx(self.code.len() - self.last_op_size?))
    }

    pub fn code_len(&self) -> usize {
        self.code.len()
    }

    /// Pop the last operation from the chunk and clean up its tracked
    /// span. Used when the compiler backtracks.
    pub fn pop_op(&mut self) {
        // Simply drop the last op.
        self.code.pop();

        if let Some(span) = self.spans.last() {
            // If the last span started at this op, drop it.
            if span.start == self.code.len() {
                self.spans.pop();
            }
        }
    }

    pub fn push_constant(&mut self, data: Value) -> ConstantIdx {
        let idx = self.constants.len();
        self.constants.push(data);
        ConstantIdx(idx)
    }

    /// Return a reference to the constant at the given [`ConstantIdx`]
    pub fn get_constant(&self, constant: ConstantIdx) -> Option<&Value> {
        self.constants.get(constant.0)
    }

    // Span tracking implementation

    fn push_span(&mut self, span: codemap::Span, start: usize) {
        match self.spans.last_mut() {
            // We do not need to insert the same span again, as this
            // instruction was compiled from the same span as the last
            // one.
            Some(last) if last.span == span => {}

            // In all other cases, this is a new source span.
            _ => self.spans.push(SourceSpan { span, start }),
        }
    }

    /// Retrieve the [codemap::Span] from which the instruction at
    /// `offset` was compiled.
    pub fn get_span(&self, offset: CodeIdx) -> codemap::Span {
        let position = self
            .spans
            .binary_search_by(|span| span.start.cmp(&offset.0));

        let span = match position {
            Ok(index) => &self.spans[index],
            Err(index) => {
                if index == 0 {
                    &self.spans[0]
                } else {
                    &self.spans[index - 1]
                }
            }
        };

        span.span
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

        match unsafe { OpCode::read(&self.code[idx.0..]).0 } {
            OpCode::OpConstant(idx) => {
                let val_str = match self.get_constant(idx) {
                    Some(Value::Thunk(t)) => t.debug_repr(),
                    Some(Value::Closure(c)) => format!("closure({:p})", c.lambda),
                    Some(val) => format!("{}", val),
                    None => format!("!!! UNKNOWN !!!"),
                };

                writeln!(writer, "OpConstant({}@{})", val_str, idx.0)
            }
            op => writeln!(writer, "{:?}", op),
        }?;

        Ok(())
    }

    pub fn op_codes(&self) -> opcode::Iter {
        unsafe { opcode::Iter::new(&self.code) }
    }

    pub fn enumerate_op_codes(&self) -> opcode::Enumerate {
        unsafe { opcode::Enumerate::new(&self.code) }
    }

    /// Extend this chunk with the content of another, moving out of the other
    /// in the process.
    ///
    /// This is used by the compiler when it detects that it unnecessarily
    /// thunked a nested expression.
    pub fn extend(&mut self, other: Self) {
        // Some operations need to be modified in certain ways before being
        // valid as part of the new chunk.
        let const_count = self.constants.len();
        for (idx, op) in other.op_codes().enumerate() {
            let span = other.get_span(CodeIdx(idx));
            match op {
                // As the constants shift, the index needs to be moved relatively.
                OpCode::OpConstant(ConstantIdx(idx)) => {
                    self.push_op(OpCode::OpConstant(ConstantIdx(idx + const_count)), span)
                }

                // Other operations either operate on relative offsets, or no
                // offsets, and are safe to keep as-is.
                _ => self.push_op(op, span),
            };
        }

        self.constants.extend(other.constants);
        self.spans.extend(other.spans);
    }

    #[inline]
    pub(crate) unsafe fn read_and_inc(&self, ip: CodeIdx) -> (OpCode, CodeIdx) {
        let (op, size) = OpCode::read(&self.code[ip.0..]);
        (op, CodeIdx(ip.0 + size))
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;

    use super::*;
    use crate::test_utils::dummy_span;

    // Note: These tests are about the functionality of the `Chunk` type, the
    // opcodes used below do *not* represent valid, executable Tvix code (and
    // don't need to).

    #[test]
    fn push_op() {
        let mut chunk = Chunk::default();
        chunk.push_op(OpCode::OpAdd, dummy_span());
        assert_eq!(*chunk.code.last().unwrap(), OpCode::OpAdd.discriminant());
    }

    #[test]
    fn extend_empty() {
        let mut chunk = Chunk::default();
        chunk.push_op(OpCode::OpAdd, dummy_span());

        let other = Chunk::default();
        chunk.extend(other);

        assert_eq!(
            chunk.code,
            vec![OpCode::OpAdd.discriminant()],
            "code should not have changed"
        );
    }

    #[test]
    fn extend_simple() {
        let span = dummy_span();
        let mut chunk = Chunk::default();
        chunk.push_op(OpCode::OpAdd, span);

        let mut other = Chunk::default();
        other.push_op(OpCode::OpSub, span);
        other.push_op(OpCode::OpMul, span);

        let expected_code = vec![OpCode::OpAdd, OpCode::OpSub, OpCode::OpMul]
            .into_iter()
            .map(|op| op.discriminant())
            .collect_vec();

        chunk.extend(other);

        assert_eq!(chunk.code, expected_code, "code should have been extended");
    }

    #[test]
    fn extend_with_constant() {
        let span = dummy_span();
        let mut chunk = Chunk::default();
        chunk.push_op(OpCode::OpAdd, span);
        let cidx = chunk.push_constant(Value::Integer(0));
        assert_eq!(
            cidx.0, 0,
            "first constant in main chunk should have index 0"
        );
        chunk.push_op(OpCode::OpConstant(cidx), span);

        let mut other = Chunk::default();
        other.push_op(OpCode::OpSub, span);
        let other_cidx = other.push_constant(Value::Integer(1));
        assert_eq!(
            other_cidx.0, 0,
            "first constant in other chunk should have index 0"
        );
        other.push_op(OpCode::OpConstant(other_cidx), span);

        chunk.extend(other);

        let expected_code = vec![
            OpCode::OpAdd,
            OpCode::OpConstant(ConstantIdx(0)),
            OpCode::OpSub,
            OpCode::OpConstant(ConstantIdx(1)), // <- note: this was rewritten
        ];

        assert_eq!(
            chunk.op_codes().collect_vec(),
            expected_code,
            "code should have been extended and rewritten"
        );

        assert_eq!(chunk.constants.len(), 2);
        assert!(matches!(chunk.constants[0], Value::Integer(0)));
        assert!(matches!(chunk.constants[1], Value::Integer(1)));
    }
}
