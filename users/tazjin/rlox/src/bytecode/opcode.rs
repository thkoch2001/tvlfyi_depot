#[derive(Debug)]
pub enum OpCode {
    /// Access a constant for use.
    OpConstant(usize),

    /// Return from the current function.
    OpReturn,

    /// Unary negation
    OpNegate,

    // Arithmetic operators
    OpAdd,
    OpSubtract,
    OpMultiply,
    OpDivide,
}
