#![allow(dead_code)]
//! https://piotrduperas.com/posts/nan-boxing

use std::ffi::c_void;
use std::fmt::{self, Debug};

#[derive(Debug, Clone, Copy)]
pub enum BoxedNanKind {
    Null,
    Bool,
    Int,
    Float,
    Ptr1,
    Ptr2,
    Ptr3,
    Ptr4,
}

#[derive(Clone, Copy)]
union BoxedNan {
    int: u64,
    float: f64,
}

impl Debug for BoxedNan {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:016x}", unsafe { self.int })
    }
}

/// Constants
impl BoxedNan {
    const QUIET_NAN: u64 = 0x7ffc000000000000;
    const NANISH_MASK: u64 = 0xffff000000000000;
    const BOOLEAN_MASK: u64 = 0x7ffe000000000002;
    const INTEGER_MASK: u64 = 0x7ffc000000000000;

    const PTR1_MASK: u64 = 0xfffc000000000000;
    const PTR2_MASK: u64 = 0xfffd000000000000;
    const PTR3_MASK: u64 = 0xfffe000000000000;
    const PTR4_MASK: u64 = 0xffff000000000000;

    const TRUE_VALUE: u64 = Self::BOOLEAN_MASK | 3;
    const FALSE_VALUE: u64 = Self::BOOLEAN_MASK | 2;
    const NULL_VALUE: u64 = 0x7ffe000000000000;
}

/// Type predicates
impl BoxedNan {
    #[inline]
    fn kind(self) -> BoxedNanKind {
        if self.is_null() {
            BoxedNanKind::Null
        } else if self.is_bool() {
            BoxedNanKind::Bool
        } else if self.is_int() {
            BoxedNanKind::Int
        } else if self.is_float() {
            BoxedNanKind::Float
        } else if self.is_ptr1() {
            BoxedNanKind::Ptr1
        } else if self.is_ptr2() {
            BoxedNanKind::Ptr2
        } else if self.is_ptr3() {
            BoxedNanKind::Ptr3
        } else if self.is_ptr4() {
            BoxedNanKind::Ptr4
        } else {
            unreachable!()
        }
    }

    #[inline]
    fn is_null(self) -> bool {
        unsafe { self.int == Self::NULL_VALUE }
    }

    #[inline]
    fn is_bool(self) -> bool {
        unsafe { (self.int & Self::BOOLEAN_MASK) == Self::BOOLEAN_MASK }
    }

    #[inline]
    fn is_int(self) -> bool {
        unsafe { (self.int & Self::NANISH_MASK) == Self::INTEGER_MASK }
    }

    #[inline]
    fn is_float(self) -> bool {
        unsafe { (self.int & Self::QUIET_NAN) != Self::QUIET_NAN }
    }

    #[inline]
    fn is_ptr1(self) -> bool {
        unsafe { (self.int & Self::NANISH_MASK) == Self::PTR1_MASK }
    }

    #[inline]
    fn is_ptr2(self) -> bool {
        unsafe { (self.int & Self::NANISH_MASK) == Self::PTR2_MASK }
    }

    #[inline]
    fn is_ptr3(self) -> bool {
        unsafe { (self.int & Self::NANISH_MASK) == Self::PTR3_MASK }
    }

    #[inline]
    fn is_ptr4(self) -> bool {
        unsafe { (self.int & Self::NANISH_MASK) == Self::PTR4_MASK }
    }
}

/// Accessors
impl BoxedNan {
    #[inline]
    unsafe fn as_bool_unchecked(self) -> bool {
        debug_assert!(self.is_bool());
        (self.int & 0x1) != 0
    }

    #[inline]
    unsafe fn as_int_unchecked(self) -> i32 {
        debug_assert!(self.is_int());
        self.int as i32
    }

    #[inline]
    unsafe fn as_float_unchecked(self) -> f64 {
        debug_assert!(self.is_float());
        self.float
    }

    #[inline]
    unsafe fn as_ptr1_unchecked(self) -> *const c_void {
        debug_assert!(self.is_ptr1());
        (self.int & 0xFFFFFFFFFFFF) as _
    }

    #[inline]
    unsafe fn as_ptr2_unchecked(self) -> *const c_void {
        debug_assert!(self.is_ptr2());
        (self.int & 0xFFFFFFFFFFFF) as _
    }

    #[inline]
    unsafe fn as_ptr3_unchecked(self) -> *const c_void {
        debug_assert!(self.is_ptr3());
        (self.int & 0xFFFFFFFFFFFF) as _
    }

    #[inline]
    unsafe fn as_ptr4_unchecked(self) -> *const c_void {
        debug_assert!(self.is_ptr4());
        (self.int & 0xFFFFFFFFFFFF) as _
    }
}

/// Constructors
impl BoxedNan {
    const NULL: Self = Self {
        int: Self::NULL_VALUE,
    };

    #[inline]
    fn int(i: i32) -> Self {
        Self {
            int: ((i as u64) & 0x00000000ffffffff) | Self::INTEGER_MASK,
        }
    }

    #[inline]
    fn float(f: f64) -> Self {
        Self { float: f }
    }

    #[inline]
    fn bool(b: bool) -> Self {
        if b {
            Self {
                int: Self::TRUE_VALUE,
            }
        } else {
            Self {
                int: Self::FALSE_VALUE,
            }
        }
    }

    #[inline]
    fn ptr1_from_raw(ptr: *const c_void) -> Self {
        Self {
            int: (ptr as u64) | Self::PTR1_MASK,
        }
    }

    #[inline]
    fn ptr2_from_raw(ptr: *const c_void) -> Self {
        Self {
            int: (ptr as u64) | Self::PTR2_MASK,
        }
    }

    #[inline]
    fn ptr3_from_raw(ptr: *const c_void) -> Self {
        Self {
            int: (ptr as u64) | Self::PTR3_MASK,
        }
    }

    #[inline]
    fn ptr4_from_raw(ptr: *const c_void) -> Self {
        Self {
            int: (ptr as u64) | Self::PTR4_MASK,
        }
    }
}

#[cfg(test)]
mod tests {
    use test_strategy::proptest;

    use super::*;

    mod value {
        use super::*;
    }

    mod boxed_nan {
        use super::*;

        fn check_kind(val: BoxedNan) {
            match val.kind() {
                BoxedNanKind::Null => assert!(val.is_null()),
                BoxedNanKind::Bool => assert!(val.is_bool()),
                BoxedNanKind::Int => assert!(val.is_int()),
                BoxedNanKind::Float => assert!(val.is_float()),
                BoxedNanKind::Ptr1 => assert!(val.is_ptr1()),
                BoxedNanKind::Ptr2 => assert!(val.is_ptr2()),
                BoxedNanKind::Ptr3 => assert!(val.is_ptr3()),
                BoxedNanKind::Ptr4 => assert!(val.is_ptr4()),
            }
        }

        #[test]
        fn null_roundtrip() {
            let val = BoxedNan::NULL;
            check_kind(val);
            assert!(val.is_null());
        }

        #[proptest]
        fn bool_roundtrip(b: bool) {
            let val = BoxedNan::bool(b);
            check_kind(val);
            assert!(val.is_bool());
            assert_eq!(unsafe { val.as_bool_unchecked() }, b);
        }

        #[test]
        fn int_roundtrip_example() {
            let val = BoxedNan::int(-1);
            check_kind(val);
            assert!(val.is_int());
            assert_eq!(unsafe { val.as_int_unchecked() }, -1);
        }

        #[proptest]
        fn int_roundtrip(i: i32) {
            let val = BoxedNan::int(i);
            check_kind(val);
            assert!(val.is_int());
            assert_eq!(unsafe { val.as_int_unchecked() }, i);
        }

        #[proptest]
        fn float_roundtrip(f: f64) {
            let val = BoxedNan::float(f);
            check_kind(val);
            assert!(val.is_float());
            assert_eq!(unsafe { val.as_float_unchecked() }, f);
        }

        #[proptest]
        fn ptr1_roundtrip(x: Box<usize>) {
            let ptr = Box::into_raw(x) as *const c_void;
            let val = BoxedNan::ptr1_from_raw(ptr);
            check_kind(val);
            assert!(val.is_ptr1());
            assert_eq!(unsafe { val.as_ptr1_unchecked() }, ptr);
            drop(unsafe { Box::from_raw(ptr as *mut usize) });
        }

        #[proptest]
        fn ptr2_roundtrip(x: Box<usize>) {
            let ptr = Box::into_raw(x) as *const c_void;
            let val = BoxedNan::ptr2_from_raw(ptr);
            check_kind(val);
            assert!(val.is_ptr2());
            assert_eq!(unsafe { val.as_ptr2_unchecked() }, ptr);
            drop(unsafe { Box::from_raw(ptr as *mut usize) });
        }

        #[proptest]
        fn ptr3_roundtrip(x: Box<usize>) {
            let ptr = Box::into_raw(x) as *const c_void;
            let val = BoxedNan::ptr3_from_raw(ptr);
            check_kind(val);
            assert!(val.is_ptr3());
            assert_eq!(unsafe { val.as_ptr3_unchecked() }, ptr);
            drop(unsafe { Box::from_raw(ptr as *mut usize) });
        }

        #[proptest]
        fn ptr4_roundtrip(x: Box<usize>) {
            let ptr = Box::into_raw(x) as *const c_void;
            let val = BoxedNan::ptr4_from_raw(ptr);
            check_kind(val);
            assert!(val.is_ptr4());
            assert_eq!(unsafe { val.as_ptr4_unchecked() }, ptr);
            drop(unsafe { Box::from_raw(ptr as *mut usize) });
        }
    }
}
