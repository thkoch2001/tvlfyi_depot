#[derive(Debug)]
pub enum OpCode {
    /// Access a constant for use.
    OpConstant(usize),

    /// Return from the current function.
    OpReturn,
}
