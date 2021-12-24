#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct HalfBytesMask(pub [u8; 16]);

impl HalfBytesMask {
    pub const B32_REVSHA256: HalfBytesMask =
        HalfBytesMask([0, 0, 0, 0, 0, 0, 255, 3, 0, 0, 0, 0, 222, 127, 207, 7]);

    pub const B64_BLAKE2B256: HalfBytesMask = HalfBytesMask([
        0, 0, 0, 0, 0, 8, 255, 3, 254, 255, 255, 135, 254, 255, 255, 7,
    ]);

    #[inline]
    #[proc_unroll::unroll]
    pub const fn from_expanded(x: [bool; 128]) -> Self {
        let mut ret = [0u8; 16];
        for idx in 0..16 {
            let mut tmp = 0;
            let fin = idx * 8;
            macro_rules! bitx {
            ($($a:expr),+) => {{ $( if x[fin + $a] { tmp += (1 << $a) as u8; } )+ }}
            }
            bitx!(0, 1, 2, 3, 4, 5, 6, 7);
            ret[idx] = tmp;
        }
        Self(ret)
    }

    /// create a mask by allowing all characters via the mask which are included in the given string
    pub fn from_bytes(s: &[u8]) -> Self {
        s.iter().fold(Self([0u8; 16]), |mut ret, &i| {
            ret.set(i, true);
            ret
        })
    }

    #[proc_unroll::unroll]
    pub const fn into_expanded(self) -> [bool; 128] {
        let Self(ihbm) = self;
        let mut ret = [false; 128];
        for idx in 0..16 {
            let fin = idx * 8;
            let curi = ihbm[idx];
            macro_rules! bitx {
                ($($a:expr),+) => {{ $( ret[fin + $a] = (curi >> $a) & 0b1 != 0; )+ }}
            }
            bitx!(0, 1, 2, 3, 4, 5, 6, 7);
        }
        ret
    }

    pub fn contains(&self, byte: u8) -> bool {
        (self.0[usize::from(byte / 8)] >> u32::from(byte % 8)) & 0b1 != 0
    }

    pub fn set(&mut self, byte: u8, allow: bool) {
        if byte >= 0x80 {
            if cfg!(debug_assertions) {
                panic!(
                    "tried to manipulate invalid byte {:?} in HalfBytesMask",
                    byte
                );
            } else {
                return;
            }
        }
        let mut block = &mut self.0[usize::from(byte / 8)];
        let bitpat = (1 << u32::from(byte % 8)) as u8;
        if allow {
            *block |= bitpat;
        } else {
            *block &= !bitpat;
        }
    }

    #[cfg(test)]
    fn count_ones(&self) -> u8 {
        self.0
            .iter()
            .map(|i| i.count_ones())
            .sum::<u32>()
            .try_into()
            .unwrap()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn maskbase() {
        assert_eq!(HalfBytesMask::B32_REVSHA256.count_ones(), 32);
        assert_eq!(HalfBytesMask::B64_BLAKE2B256.count_ones(), 64);
    }

    #[test]
    fn dflmask() {
        assert_eq!(
            HalfBytesMask::from_expanded(
                [
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                ]
                .map(|i| i != 0)
            ),
            Default::default(),
        );

        assert_eq!(
            HalfBytesMask::from_expanded(
                [
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1,
                    1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 1, 1,
                    1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0,
                ]
                .map(|i| i != 0)
            ),
            HalfBytesMask::B32_REVSHA256,
        );

        assert_eq!(
            HalfBytesMask::from_expanded(
                [
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 1, 1,
                    1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1,
                    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0,
                ]
                .map(|i| i != 0)
            ),
            HalfBytesMask::B64_BLAKE2B256,
        );
    }

    proptest::proptest! {
        #[test]
        fn hbm_roundtrip(s: [u8; 16]) {
            let a = HalfBytesMask(s);
            let b = a.into_expanded();
            let c = HalfBytesMask::from_expanded(b);
            assert_eq!(a, c);
        }
    }
}
