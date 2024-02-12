#![allow(dead_code)]
//! https://piotrduperas.com/posts/nan-boxing

use std::fmt::{self, Debug};
use std::marker::PhantomData;
use std::mem::ManuallyDrop;
use std::ptr::NonNull;

use erasable::{ErasablePtr, ErasedPtr};

#[derive(Debug, Clone, Copy)]
pub enum BoxedNanKind {
    Null,
    Bool,
    Int,
    Float,
    PtrA,
    PtrB,
    PtrC,
    PtrD,
}

#[derive(Clone)]
pub enum NeverPtr {}
unsafe impl ErasablePtr for NeverPtr {
    fn erase(this: Self) -> erasable::ErasedPtr {
        match this {}
    }

    unsafe fn unerase(_this: erasable::ErasedPtr) -> Self {
        unreachable!()
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum Enum4<A, B, C, D> {
    A(A),
    B(B),
    C(C),
    D(D),
}

pub union BoxedNan<A = NeverPtr, B = NeverPtr, C = NeverPtr, D = NeverPtr>
where
    A: ErasablePtr,
    B: ErasablePtr,
    C: ErasablePtr,
    D: ErasablePtr,
{
    int: u64,
    float: f64,
    _phantom: PhantomData<Enum4<A, B, C, D>>,
}

impl Debug for BoxedNan {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:016x}", unsafe { self.int })
    }
}

impl<A, B, C, D> Drop for BoxedNan<A, B, C, D>
where
    A: ErasablePtr,
    B: ErasablePtr,
    C: ErasablePtr,
    D: ErasablePtr,
{
    fn drop(&mut self) {
        match self.kind() {
            BoxedNanKind::Null | BoxedNanKind::Bool | BoxedNanKind::Int | BoxedNanKind::Float => {}
            BoxedNanKind::PtrA => unsafe {
                drop(A::unerase(NonNull::new_unchecked(
                    (self.int & 0xFFFFFFFFFFFF) as *mut _,
                )));
            },
            BoxedNanKind::PtrB => unsafe {
                drop(B::unerase(NonNull::new_unchecked(
                    (self.int & 0xFFFFFFFFFFFF) as *mut _,
                )));
            },
            BoxedNanKind::PtrC => unsafe {
                drop(C::unerase(NonNull::new_unchecked(
                    (self.int & 0xFFFFFFFFFFFF) as *mut _,
                )));
            },
            BoxedNanKind::PtrD => unsafe {
                drop(D::unerase(NonNull::new_unchecked(
                    (self.int & 0xFFFFFFFFFFFF) as *mut _,
                )));
            },
        }
    }
}

impl<A, B, C, D> Clone for BoxedNan<A, B, C, D>
where
    A: ErasablePtr + Clone,
    B: ErasablePtr + Clone,
    C: ErasablePtr + Clone,
    D: ErasablePtr + Clone,
{
    fn clone(&self) -> Self {
        match self.kind() {
            BoxedNanKind::Null => Self::NULL,
            BoxedNanKind::Bool => Self {
                int: unsafe { self.int },
            },
            BoxedNanKind::Int => Self {
                int: unsafe { self.int },
            },
            BoxedNanKind::Float => Self {
                float: unsafe { self.float },
            },
            BoxedNanKind::PtrA => unsafe {
                Self::ptra(ManuallyDrop::into_inner(
                    ManuallyDrop::new(A::unerase(self.as_ptr_unchecked())).clone(),
                ))
            },
            BoxedNanKind::PtrB => unsafe {
                Self::ptrb(ManuallyDrop::into_inner(
                    ManuallyDrop::new(B::unerase(self.as_ptr_unchecked())).clone(),
                ))
            },
            BoxedNanKind::PtrC => unsafe {
                Self::ptrc(ManuallyDrop::into_inner(
                    ManuallyDrop::new(C::unerase(self.as_ptr_unchecked())).clone(),
                ))
            },
            BoxedNanKind::PtrD => unsafe {
                Self::ptrd(ManuallyDrop::into_inner(
                    ManuallyDrop::new(D::unerase(self.as_ptr_unchecked())).clone(),
                ))
            },
        }
    }
}

const QUIET_NAN: u64 = 0x7ffc000000000000;
const NANISH_MASK: u64 = 0xffff000000000000;
const BOOLEAN_MASK: u64 = 0x7ffe000000000002;
const INTEGER_MASK: u64 = 0x7ffc000000000000;

const PTRA_MASK: u64 = 0xfffc000000000000;
const PTRB_MASK: u64 = 0xfffd000000000000;
const PTRC_MASK: u64 = 0xfffe000000000000;
const PTRD_MASK: u64 = 0xffff000000000000;

const TRUE_VALUE: u64 = BOOLEAN_MASK | 3;
const FALSE_VALUE: u64 = BOOLEAN_MASK | 2;
const NULL_VALUE: u64 = 0x7ffe000000000000;

/// Type predicates
impl<A, B, C, D> BoxedNan<A, B, C, D>
where
    A: ErasablePtr,
    B: ErasablePtr,
    C: ErasablePtr,
    D: ErasablePtr,
{
    #[inline]
    pub fn kind(&self) -> BoxedNanKind {
        if self.is_null() {
            BoxedNanKind::Null
        } else if self.is_bool() {
            BoxedNanKind::Bool
        } else if self.is_int() {
            BoxedNanKind::Int
        } else if self.is_float() {
            BoxedNanKind::Float
        } else if self.is_ptra() {
            BoxedNanKind::PtrA
        } else if self.is_ptrb() {
            BoxedNanKind::PtrB
        } else if self.is_ptrc() {
            BoxedNanKind::PtrC
        } else if self.is_ptrd() {
            BoxedNanKind::PtrD
        } else {
            unreachable!()
        }
    }

    #[inline]
    pub fn is_null(&self) -> bool {
        unsafe { self.int == NULL_VALUE }
    }

    #[inline]
    pub fn is_bool(&self) -> bool {
        unsafe { (self.int & BOOLEAN_MASK) == BOOLEAN_MASK }
    }

    #[inline]
    pub fn is_int(&self) -> bool {
        unsafe { (self.int & NANISH_MASK) == INTEGER_MASK }
    }

    #[inline]
    pub fn is_float(&self) -> bool {
        unsafe { (self.int & QUIET_NAN) != QUIET_NAN }
    }

    #[inline]
    pub fn is_ptra(&self) -> bool {
        unsafe { (self.int & NANISH_MASK) == PTRA_MASK }
    }

    #[inline]
    pub fn is_ptrb(&self) -> bool {
        unsafe { (self.int & NANISH_MASK) == PTRB_MASK }
    }

    #[inline]
    pub fn is_ptrc(&self) -> bool {
        unsafe { (self.int & NANISH_MASK) == PTRC_MASK }
    }

    #[inline]
    pub fn is_ptrd(&self) -> bool {
        unsafe { (self.int & NANISH_MASK) == PTRD_MASK }
    }
}

/// Accessors
impl<A, B, C, D> BoxedNan<A, B, C, D>
where
    A: ErasablePtr,
    B: ErasablePtr,
    C: ErasablePtr,
    D: ErasablePtr,
{
    #[inline]
    pub unsafe fn as_bool_unchecked(&self) -> bool {
        debug_assert!(self.is_bool());
        (self.int & 0x1) != 0
    }

    #[inline]
    pub unsafe fn as_int_unchecked(&self) -> i32 {
        debug_assert!(self.is_int());
        self.int as i32
    }

    #[inline]
    pub unsafe fn as_float_unchecked(&self) -> f64 {
        debug_assert!(self.is_float());
        self.float
    }

    #[inline]
    pub unsafe fn as_ptr_unchecked(&self) -> ErasedPtr {
        NonNull::new_unchecked((self.int & 0xFFFFFFFFFFFF) as *mut _)
    }

    #[inline]
    pub unsafe fn into_ptra_unchecked(self) -> A {
        debug_assert!(self.is_ptra());
        A::unerase(ManuallyDrop::new(self).as_ptr_unchecked())
    }

    #[inline]
    pub unsafe fn as_ref_a_unchecked<T>(&self) -> &T
    where
        A: AsRef<T>,
    {
        debug_assert!(self.is_ptra());
        NonNull::from(ManuallyDrop::new(A::unerase(self.as_ptr_unchecked())).as_ref()).as_ref()
    }

    #[inline]
    pub unsafe fn as_mut_a_unchecked<T>(&mut self) -> &mut T
    where
        A: AsMut<T>,
    {
        debug_assert!(self.is_ptra());
        NonNull::from(ManuallyDrop::new(A::unerase(self.as_ptr_unchecked())).as_mut()).as_mut()
    }

    #[inline]
    pub unsafe fn into_ptrb_unchecked(self) -> B {
        debug_assert!(self.is_ptrb());
        B::unerase(NonNull::new_unchecked(
            (ManuallyDrop::new(self).int & 0xFFFFFFFFFFFF) as *mut _,
        ))
    }

    #[inline]
    pub unsafe fn as_ref_b_unchecked<T>(&self) -> &T
    where
        B: AsRef<T>,
    {
        debug_assert!(self.is_ptrb());
        NonNull::from(ManuallyDrop::new(B::unerase(self.as_ptr_unchecked())).as_ref()).as_ref()
    }

    #[inline]
    pub unsafe fn as_mut_b_unchecked<T>(&mut self) -> &mut T
    where
        B: AsMut<T>,
    {
        debug_assert!(self.is_ptrb());
        NonNull::from(ManuallyDrop::new(B::unerase(self.as_ptr_unchecked())).as_mut()).as_mut()
    }

    #[inline]
    pub unsafe fn into_ptrc_unchecked(self) -> C {
        debug_assert!(self.is_ptrc());
        C::unerase(NonNull::new_unchecked(
            (ManuallyDrop::new(self).int & 0xFFFFFFFFFFFF) as *mut _,
        ))
    }

    #[inline]
    pub unsafe fn as_ref_c_unchecked<T>(&self) -> &T
    where
        C: AsRef<T>,
    {
        debug_assert!(self.is_ptrc());
        NonNull::from(ManuallyDrop::new(C::unerase(self.as_ptr_unchecked())).as_ref()).as_ref()
    }

    #[inline]
    pub unsafe fn as_mut_c_unchecked<T>(&mut self) -> &mut T
    where
        C: AsMut<T>,
    {
        debug_assert!(self.is_ptrc());
        NonNull::from(ManuallyDrop::new(C::unerase(self.as_ptr_unchecked())).as_mut()).as_mut()
    }

    #[inline]
    pub unsafe fn into_ptrd_unchecked(self) -> D {
        debug_assert!(self.is_ptrd());
        D::unerase(NonNull::new_unchecked(
            (ManuallyDrop::new(self).int & 0xFFFFFFFFFFFF) as *mut _,
        ))
    }

    #[inline]
    pub unsafe fn as_ref_d_unchecked<T>(&self) -> &T
    where
        D: AsRef<T>,
    {
        debug_assert!(self.is_ptrd());
        NonNull::from(ManuallyDrop::new(D::unerase(self.as_ptr_unchecked())).as_ref()).as_ref()
    }

    #[inline]
    pub unsafe fn as_mut_d_unchecked<T>(&mut self) -> &mut T
    where
        D: AsMut<T>,
    {
        debug_assert!(self.is_ptrd());
        NonNull::from(ManuallyDrop::new(D::unerase(self.as_ptr_unchecked())).as_mut()).as_mut()
    }
}

/// Constructors
impl<A, B, C, D> BoxedNan<A, B, C, D>
where
    A: ErasablePtr,
    B: ErasablePtr,
    C: ErasablePtr,
    D: ErasablePtr,
{
    pub const NULL: Self = Self { int: NULL_VALUE };

    #[inline]
    pub fn int(i: i32) -> Self {
        Self {
            int: ((i as u64) & 0x00000000ffffffff) | INTEGER_MASK,
        }
    }

    #[inline]
    pub fn float(f: f64) -> Self {
        Self { float: f }
    }

    #[inline]
    pub fn bool(b: bool) -> Self {
        if b {
            Self { int: TRUE_VALUE }
        } else {
            Self { int: FALSE_VALUE }
        }
    }

    #[inline]
    pub fn ptra_from_raw(ptr: ErasedPtr) -> Self {
        Self {
            int: (ptr.as_ptr() as u64) | PTRA_MASK,
        }
    }

    #[inline]
    pub fn ptra(ptr: A) -> Self {
        Self::ptra_from_raw(A::erase(ptr))
    }

    #[inline]
    pub fn ptrb_from_raw(ptr: ErasedPtr) -> Self {
        Self {
            int: (ptr.as_ptr() as u64) | PTRB_MASK,
        }
    }

    #[inline]
    pub fn ptrb(ptr: B) -> Self {
        Self::ptrb_from_raw(B::erase(ptr))
    }

    #[inline]
    pub fn ptrc_from_raw(ptr: ErasedPtr) -> Self {
        Self {
            int: (ptr.as_ptr() as u64) | PTRC_MASK,
        }
    }

    #[inline]
    pub fn ptrc(ptr: C) -> Self {
        Self::ptrc_from_raw(C::erase(ptr))
    }

    #[inline]
    pub fn ptrd_from_raw(ptr: ErasedPtr) -> Self {
        Self {
            int: (ptr.as_ptr() as u64) | PTRD_MASK,
        }
    }

    #[inline]
    pub fn ptrd(ptr: D) -> Self {
        Self::ptrd_from_raw(D::erase(ptr))
    }
}

#[cfg(test)]
mod tests {
    use test_strategy::proptest;

    use super::*;

    mod boxed_nan {
        use super::*;

        fn check_kind<A, B, C, D>(val: &BoxedNan<A, B, C, D>)
        where
            A: ErasablePtr,
            B: ErasablePtr,
            C: ErasablePtr,
            D: ErasablePtr,
        {
            match val.kind() {
                BoxedNanKind::Null => assert!(val.is_null()),
                BoxedNanKind::Bool => assert!(val.is_bool()),
                BoxedNanKind::Int => assert!(val.is_int()),
                BoxedNanKind::Float => assert!(val.is_float()),
                BoxedNanKind::PtrA => assert!(val.is_ptra()),
                BoxedNanKind::PtrB => assert!(val.is_ptrb()),
                BoxedNanKind::PtrC => assert!(val.is_ptrc()),
                BoxedNanKind::PtrD => assert!(val.is_ptrd()),
            }
        }

        #[test]
        fn null_roundtrip() {
            let val: BoxedNan = BoxedNan::NULL;
            check_kind(&val);
            assert!(val.is_null());
        }

        #[proptest]
        fn bool_roundtrip(b: bool) {
            let val: BoxedNan = BoxedNan::bool(b);
            check_kind(&val);
            assert!(val.is_bool());
            assert_eq!(unsafe { val.as_bool_unchecked() }, b);
        }

        #[test]
        fn int_roundtrip_example() {
            let val: BoxedNan = BoxedNan::int(-1);
            check_kind(&val);
            assert!(val.is_int());
            assert_eq!(unsafe { val.as_int_unchecked() }, -1);
        }

        #[proptest]
        fn int_roundtrip(i: i32) {
            let val: BoxedNan = BoxedNan::int(i);
            check_kind(&val);
            assert!(val.is_int());
            assert_eq!(unsafe { val.as_int_unchecked() }, i);
        }

        #[proptest]
        fn float_roundtrip(f: f64) {
            let val: BoxedNan = BoxedNan::float(f);
            check_kind(&val);
            assert!(val.is_float());
            assert_eq!(unsafe { val.as_float_unchecked() }, f);
        }

        #[proptest]
        fn ptra_roundtrip(x: Box<usize>) {
            let val: BoxedNan<_> = BoxedNan::ptra(x.clone());
            check_kind(&val);
            assert!(val.is_ptra());
            assert_eq!(*unsafe { val.as_ref_a_unchecked() }, *x);
            assert_eq!(unsafe { val.into_ptra_unchecked() }, x);
        }

        #[proptest]
        fn ptrb_roundtrip(x: Box<usize>) {
            let val: BoxedNan<NeverPtr, _> = BoxedNan::ptrb(x.clone());
            check_kind(&val);
            assert!(val.is_ptrb());
            assert_eq!(*unsafe { val.as_ref_b_unchecked() }, *x);
            assert_eq!(unsafe { val.into_ptrb_unchecked() }, x);
        }

        #[proptest]
        fn ptrc_roundtrip(x: Box<usize>) {
            let val: BoxedNan<NeverPtr, NeverPtr, _> = BoxedNan::ptrc(x.clone());
            check_kind(&val);
            assert!(val.is_ptrc());
            assert_eq!(*unsafe { val.as_ref_c_unchecked() }, *x);
            assert_eq!(unsafe { val.into_ptrc_unchecked() }, x);
        }

        #[proptest]
        fn ptrd_roundtrip(x: Box<usize>) {
            let val: BoxedNan<NeverPtr, NeverPtr, NeverPtr, _> = BoxedNan::ptrd(x.clone());
            check_kind(&val);
            assert!(val.is_ptrd());
            assert_eq!(*unsafe { val.as_ref_d_unchecked() }, *x);
            assert_eq!(unsafe { val.into_ptrd_unchecked() }, x);
        }

        #[proptest]
        fn clone_ptra(x: Box<usize>) {
            let val: BoxedNan<_> = BoxedNan::ptra(x.clone());
            let val2 = val.clone();
            assert_eq!(unsafe { val2.into_ptra_unchecked() }, x);
        }

        #[proptest]
        fn clone_ptrb(x: Box<usize>) {
            let val: BoxedNan<NeverPtr, _> = BoxedNan::ptrb(x.clone());
            let val2 = val.clone();
            assert_eq!(unsafe { val2.into_ptrb_unchecked() }, x);
        }

        #[proptest]
        fn clone_ptrc(x: Box<usize>) {
            let val: BoxedNan<NeverPtr, NeverPtr, _> = BoxedNan::ptrc(x.clone());
            let val2 = val.clone();
            assert_eq!(unsafe { val2.into_ptrc_unchecked() }, x);
        }

        #[proptest]
        fn clone_ptrd(x: Box<usize>) {
            let val: BoxedNan<NeverPtr, NeverPtr, NeverPtr, _> = BoxedNan::ptrd(x.clone());
            let val2 = val.clone();
            assert_eq!(unsafe { val2.into_ptrd_unchecked() }, x);
        }
    }
}
