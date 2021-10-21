#[derive(Clone, Copy, Debug)]
pub struct ConstantIdx(pub usize);

#[derive(Clone, Copy, Debug)]
pub struct StackIdx(pub usize);

#[derive(Clone, Copy, Debug)]
pub struct CodeIdx(pub usize);

#[derive(Clone, Copy, Debug)]
pub struct CodeOffset(pub usize);

#[derive(Debug)]
pub enum OpCode {
    /// Push a constant onto the stack.
    OpConstant(ConstantIdx),

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
    OpDefineGlobal(ConstantIdx),
    OpGetGlobal(ConstantIdx),
    OpSetGlobal(ConstantIdx),
    OpGetLocal(StackIdx),
    OpSetLocal(StackIdx),

    // Control flow
    OpJumpPlaceholder(bool),
    OpJumpIfFalse(CodeOffset),
}
