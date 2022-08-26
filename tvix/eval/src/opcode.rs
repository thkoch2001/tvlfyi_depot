//! This module implements the instruction set running on the abstract
//! machine implemented by tvix.

/// Index of a constant in the current code chunk.
#[repr(transparent)]
#[derive(Clone, Copy, Debug)]
pub struct ConstantIdx(pub usize);

/// Index of an instruction in the current code chunk.
#[repr(transparent)]
#[derive(Clone, Copy, Debug)]
pub struct CodeIdx(pub usize);

/// Index of a value in the runtime stack.
#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct StackIdx(pub usize);

/// Index of an upvalue within a closure's upvalue list.
#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct UpvalueIdx(pub usize);

/// Offset by which an instruction pointer should change in a jump.
#[repr(transparent)]
#[derive(Clone, Copy, Debug)]
pub struct JumpOffset(pub usize);

/// Provided count for an instruction (could represent e.g. a number
/// of elements).
#[repr(transparent)]
#[derive(Clone, Copy, Debug)]
pub struct Count(pub usize);

#[allow(clippy::enum_variant_names)]
#[warn(variant_size_differences)]
#[derive(Clone, Copy, Debug)]
pub enum OpCode {
    // Push a constant onto the stack.
    OpConstant(ConstantIdx),

    // Discard a value from the stack.
    OpPop,

    // Push a literal value.
    OpNull,
    OpTrue,
    OpFalse,

    // Unary operators
    OpInvert,
    OpNegate,

    // Arithmetic binary operators
    OpAdd,
    OpSub,
    OpMul,
    OpDiv,

    // Comparison operators
    OpEqual,
    OpLess,
    OpLessOrEq,
    OpMore,
    OpMoreOrEq,

    // Logical operators & generic jumps
    OpJump(JumpOffset),
    OpJumpIfTrue(JumpOffset),
    OpJumpIfFalse(JumpOffset),
    OpJumpIfNotFound(JumpOffset),

    // Attribute sets
    OpAttrs(Count),
    OpAttrPath(Count),
    OpAttrsUpdate,
    OpAttrsSelect,
    OpAttrsTrySelect,
    OpAttrsIsSet,

    // `with`-handling
    OpPushWith(StackIdx),
    OpPopWith,
    OpResolveWith,

    // Lists
    OpList(Count),
    OpConcat,

    // Strings
    OpInterpolate(Count),

    // Type assertion operators
    OpAssertBool,

    // Access local identifiers with statically known positions.
    OpGetLocal(StackIdx),

    // Close scopes while leaving their expression value around.
    OpCloseScope(Count), // number of locals to pop

    // Asserts stack top is a boolean, and true.
    OpAssert,

    // Lambdas
    OpCall,
    OpGetUpvalue(UpvalueIdx),
}
