use std::io::Write;
use std::ops::{Index, IndexMut};

use crate::opcode::{CodeIdx, ConstantIdx, OpCode};
use crate::value::Value;
use crate::SourceCode;

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

/// A chunk is a representation of a sequence of bytecode
/// instructions, associated constants and additional metadata as
/// emitted by the compiler.
#[derive(Debug, Default)]
pub struct Chunk {
    pub code: Vec<OpCode>,
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
        &self.code[index.0]
    }
}

impl IndexMut<CodeIdx> for Chunk {
    fn index_mut(&mut self, index: CodeIdx) -> &mut Self::Output {
        &mut self.code[index.0]
    }
}

impl Chunk {
    pub fn push_op(&mut self, data: OpCode, span: codemap::Span) -> CodeIdx {
        let idx = self.code.len();
        self.code.push(data);
        self.push_span(span);
        CodeIdx(idx)
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

    /// Extend this chunk with the content of another, moving out of the other
    /// in the process.
    ///
    /// This is used by the compiler when it detects that it unnecessarily
    /// thunked a nested expression.
    pub fn extend(&mut self, other: Self) {
        // Some operations need to be modified in certain ways before being
        // valid as part of the new chunk.
        let const_count = self.constants.len();
        for (idx, op) in other.code.iter().enumerate() {
            let span = other.get_span(CodeIdx(idx));
            match op {
                // As the constants shift, the index needs to be moved relatively.
                OpCode::OpConstant(ConstantIdx(idx)) => {
                    self.push_op(OpCode::OpConstant(ConstantIdx(idx + const_count)), span)
                }

                // Other operations either operate on relative offsets, or no
                // offsets, and are safe to keep as-is.
                _ => self.push_op(*op, span),
            };
        }

        self.constants.extend(other.constants);
        self.spans.extend(other.spans);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::dummy_span;

    // Note: These tests are about the functionality of the `Chunk` type, the
    // opcodes used below do *not* represent valid, executable Tvix code (and
    // don't need to).

    #[test]
    fn push_op() {
        let mut chunk = Chunk::default();
        chunk.push_op(OpCode::OpAdd, dummy_span());
        assert_eq!(chunk.code.last().unwrap(), &OpCode::OpAdd);
    }

    #[test]
    fn extend_empty() {
        let mut chunk = Chunk::default();
        chunk.push_op(OpCode::OpAdd, dummy_span());

        let other = Chunk::default();
        chunk.extend(other);

        assert_eq!(
            chunk.code,
            vec![OpCode::OpAdd],
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

        let expected_code = vec![OpCode::OpAdd, OpCode::OpSub, OpCode::OpMul];

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
            chunk.code, expected_code,
            "code should have been extended and rewritten"
        );

        assert_eq!(chunk.constants.len(), 2);
        assert!(matches!(chunk.constants[0], Value::Integer(0)));
        assert!(matches!(chunk.constants[1], Value::Integer(1)));
    }
}
