//! This module implements the instruction set running on the abstract
//! machine implemented by tvix.

use std::ops::{AddAssign, Sub};

/// Index of a constant in the current code chunk.
#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ConstantIdx(pub usize);

/// Index of an instruction in the current code chunk.
#[repr(transparent)]
#[derive(Clone, Copy, Debug, Default)]
pub struct CodeIdx(pub usize);

impl AddAssign<usize> for CodeIdx {
    fn add_assign(&mut self, rhs: usize) {
        *self = CodeIdx(self.0 + rhs)
    }
}

impl Sub<usize> for CodeIdx {
    type Output = Self;

    fn sub(self, rhs: usize) -> Self::Output {
        CodeIdx(self.0 - rhs)
    }
}

/// Index of a value in the runtime stack.  This is an offset
/// *relative to* the VM value stack_base of the CallFrame
/// containing the opcode which contains this StackIdx.
#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd)]
pub struct StackIdx(pub usize);

/// Index of an upvalue within a closure's bound-variable upvalue
/// list.  This is an absolute index into the Upvalues of the
/// CallFrame containing the opcode which contains this UpvalueIdx.
#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct UpvalueIdx(pub usize);

/// Offset by which an instruction pointer should change in a jump.
#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct JumpOffset(pub usize);

/// Provided count for an instruction (could represent e.g. a number
/// of elements).
#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Count(pub usize);

/// All variants of this enum carry a bounded amount of data to
/// ensure that no heap allocations are needed for an Opcode.
///
/// In documentation comments, stack positions are referred to by
/// indices written in `{}` as such, where required:
///
/// ```notrust
///                             --- top of the stack
///                            /
///                           v
///       [ ... | 3 | 2 | 1 | 0 ]
///                   ^
///                  /
/// 2 values deep ---
/// ```
///
/// Unless otherwise specified, operations leave their result at the
/// top of the stack.
#[warn(variant_size_differences)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum OpCode {
    /// Push a constant onto the stack. Followed by [usize] operand
    /// with the constant's index in the chunk.
    OpConstant,

    // Unary operators
    /// Discard a value from the stack.
    OpPop,

    /// Invert the boolean at the top of the stack.
    OpInvert,

    // Binary operators
    /// Invert the sign of the number at the top of the stack.
    OpNegate,

    /// Sum up the two numbers at the top of the stack.
    OpAdd,

    /// Subtract the number at {1} from the number at {2}.
    OpSub,

    /// Multiply the two numbers at the top of the stack.
    OpMul,

    /// Divide the two numbers at the top of the stack.
    OpDiv,

    // Comparison operators
    /// Check the two values at the top of the stack for Nix-equality.
    OpEqual,

    /// Check whether the value at {2} is less than {1}.
    OpLess,

    /// Check whether the value at {2} is less than or equal to {1}.
    OpLessOrEq,

    /// Check whether the value at {2} is greater than {1}.
    OpMore,

    /// Check whether the value at {2} is greater than or equal to {1}.
    OpMoreOrEq,

    // Logical operators & generic jumps
    /// Jump forward in the bytecode specified by the number of
    /// instructions in its usize operand.
    OpJump,

    /// Jump forward in the bytecode specified by the number of
    /// instructions in its usize operand, *if* the value at the top
    /// of the stack is `true`.
    OpJumpIfTrue,

    /// Jump forward in the bytecode specified by the number of
    /// instructions in its usize operand, *if* the value at the top
    /// of the stack is `false`.
    OpJumpIfFalse,

    /// Jump forward in the bytecode specified by the number of
    /// instructions in its usize operand, *if* the value at the top
    /// of the stack is the internal value representing a missing
    /// attribute set key.
    OpJumpIfNotFound,

    // Attribute sets
    /// Construct an attribute set from key-value pairs on the top of the stack.
    /// The number of pairs is given as a usize operand.
    ///
    /// Note that this takes the count of *pairs*, not the number of *stack
    /// values* - the actual number of values popped off the stack will be twice
    /// the operand to this op.
    OpAttrs,

    /// Merge the attribute set at {2} into the attribute set at {1},
    /// and leave the new set at the top of the stack.
    OpAttrsUpdate,

    /// Select the attribute with the name at {1} from the set at {2}.
    OpAttrsSelect,

    /// Select the attribute with the name at {1} from the set at {2}, but leave
    /// a `Value::AttrNotFound` in the stack instead of failing if it is
    /// missing.
    OpAttrsTrySelect,

    /// Check for the presence of the attribute with the name at {1} in the set
    /// at {2}.
    OpHasAttr,

    /// Throw an error if the attribute set at the top of the stack has any attributes
    /// other than those listed in the formals of the current lambda
    ///
    /// Panics if the current frame is not a lambda with formals
    OpValidateClosedFormals,

    // `with`-handling
    /// Push a value onto the runtime `with`-stack to enable dynamic identifier
    /// resolution. The absolute stack index of the value is supplied as a usize
    /// operand.
    OpPushWith,

    /// Pop the last runtime `with`-stack element.
    OpPopWith,

    /// Dynamically resolve an identifier with the name at {1} from the runtime
    /// `with`-stack.
    OpResolveWith,

    // Lists
    /// Construct a list from the given number of values at the top of the
    /// stack.
    OpList,

    /// Concatenate the lists at {2} and {1}.
    OpConcat,

    // Strings
    /// Interpolate the given number of string fragments into a single string.
    OpInterpolate,

    /// Force the Value on the stack and coerce it to a string, always using
    /// `CoercionKind::Weak`.
    OpCoerceToString,

    // Paths
    /// Attempt to resolve the Value on the stack using the configured [`NixSearchPath`][]
    ///
    /// [`NixSearchPath`]: crate::nix_search_path::NixSearchPath
    OpFindFile,

    /// Attempt to resolve a path literal relative to the home dir
    OpResolveHomePath,

    // Type assertion operators
    /// Assert that the value at {1} is a boolean, and fail with a runtime error
    /// otherwise.
    OpAssertBool,

    /// Access local identifiers with statically known positions.
    OpGetLocal,

    /// Close scopes while leaving their expression value around.
    OpCloseScope, // number of locals to pop

    /// Return an error indicating that an `assert` failed
    OpAssertFail,

    // Lambdas & closures
    /// Call the value at {1} in a new VM callframe
    OpCall,

    /// Call the value at {1} in the same VM callframe.
    OpTailCall,

    /// Retrieve the upvalue at the given index from the closure or thunk
    /// currently under evaluation.
    OpGetUpvalue(UpvalueIdx),

    /// Construct a closure which has upvalues but no self-references
    OpClosure(ConstantIdx),

    /// Construct a closure which has self-references (direct or via upvalues)
    OpThunkClosure(ConstantIdx),

    /// Construct a suspended thunk, used to delay a computation for laziness.
    OpThunkSuspended(ConstantIdx),

    /// Force the value at {1} until it is a `Thunk::Evaluated`.
    OpForce,

    /// Finalise initialisation of the upvalues of the value in the given stack
    /// index (which must be a Value::Thunk) after the scope is fully bound.
    OpFinalise(StackIdx),

    // [`OpClosure`], [`OpThunkSuspended`], and [`OpThunkClosure`] have a
    // variable number of arguments to the instruction, which is
    // represented here by making their data part of the opcodes.
    // Each of these two opcodes has a `ConstantIdx`, which must
    // reference a `Value::Blueprint(Lambda)`.  The `upvalue_count`
    // field in that `Lambda` indicates the number of arguments it
    // takes, and the opcode must be followed by exactly this number
    // of `Data*` opcodes.  The VM skips over these by advancing the
    // instruction pointer.
    //
    // It is illegal for a `Data*` opcode to appear anywhere else.
    /// Populate a static upvalue by copying from the stack immediately.
    DataStackIdx(StackIdx),
    /// Populate a static upvalue of a thunk by copying it the stack, but do
    /// when the thunk is finalised (by OpFinalise) rather than immediately.
    DataDeferredLocal(StackIdx),
    /// Populate a static upvalue by copying it from the upvalues of an
    /// enclosing scope.
    DataUpvalueIdx(UpvalueIdx),
    /// Populate dynamic upvalues by saving a copy of the with-stack.
    DataCaptureWith,
}
