use std::mem;

pub use opcode_macros::PackedEncode;

pub trait PackedEncode: Sized {
    fn push(&self, out: &mut Vec<u8>);
    unsafe fn read(data: &[u8]) -> (Self, usize);
}

impl PackedEncode for usize {
    #[inline]
    fn push(&self, out: &mut Vec<u8>) {
        out.extend(self.to_ne_bytes());
    }

    #[inline]
    unsafe fn read(data: &[u8]) -> (Self, usize) {
        let len = mem::size_of::<usize>();
        let res = usize::from_ne_bytes(data[0..len].try_into().unwrap_unchecked());
        (res, len)
    }
}

impl PackedEncode for bool {
    #[inline]
    fn push(&self, out: &mut Vec<u8>) {
        out.push(*self as u8);
    }

    #[inline]
    unsafe fn read(data: &[u8]) -> (Self, usize) {
        (data[0] != 0, 1)
    }
}

#[cfg(test)]
mod tests {
    use test_strategy::proptest;

    use super::*;

    #[proptest]
    fn usize_round_trip(val: usize) {
        let mut encoded = vec![];
        val.push(&mut encoded);
        let (res, len) = unsafe { usize::read(&encoded) };
        assert_eq!(res, val);
        assert_eq!(len, encoded.len());
    }
}
