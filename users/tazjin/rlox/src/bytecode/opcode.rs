#[derive(Debug)]
pub enum OpCode {
    /// Push a constant onto the stack.
    OpConstant(usize),

    // Literal pushes
    OpNil,
    OpTrue,
    OpFalse,

    /// Return from the current function.
    OpReturn,

    // Boolean & comparison operators
    OpNot,
    OpEqual,
    OpGreater,
    OpLess,

    /// Unary negation
    OpNegate,

    // Arithmetic operators
    OpAdd,
    OpSubtract,
    OpMultiply,
    OpDivide,

    // Built in operations
    OpPrint,
    OpPop,

    // Variable management
    OpDefineGlobal(usize),
    OpGetGlobal(usize),
    OpSetGlobal(usize),
}
