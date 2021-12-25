#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct HalfBytesMask(pub [u8; 16]);

#[allow(clippy::as_conversions, clippy::zero_prefixed_literal)]
impl HalfBytesMask {
    pub const B32_REVSHA256: HalfBytesMask =
        HalfBytesMask([0, 0, 0, 0, 0, 0, 255, 3, 0, 0, 0, 0, 222, 127, 207, 7]);

    pub const B64_BLAKE2B256: HalfBytesMask = HalfBytesMask([
        0, 0, 0, 0, 0, 8, 255, 3, 254, 255, 255, 135, 254, 255, 255, 7,
    ]);

    pub const DFL_REST: HalfBytesMask = HalfBytesMask([
        0, 0, 0, 0, 0, 104, 255, 163, 254, 255, 255, 135, 254, 255, 255, 7,
    ]);

    #[inline]
    pub const fn from_expanded(x: [bool; 128]) -> Self {
        let mut ret = [0u8; 16];
        let mut idx = 0;
        while idx < 16 {
            let fin = idx * 8;
            let mut idx2 = 0;
            while idx2 < 8 {
                if x[fin + idx2] {
                    ret[idx] += (1 << idx2) as u8;
                }
                idx2 += 1;
            }
            idx += 1;
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

    pub const fn into_expanded(self) -> [bool; 128] {
        let Self(ihbm) = self;
        let mut ret = [false; 128];
        let mut idx = 0;
        while idx < 16 {
            let fin = idx * 8;
            let curi = ihbm[idx];
            let mut idx2 = 0;
            while idx2 < 8 {
                ret[fin + idx2] = (curi >> idx2) & 0b1 != 0;
                idx2 += 1;
            }
            idx += 1;
        }
        ret
    }

    pub fn contains(&self, byte: u8) -> bool {
        if byte >= 0x80 {
            false
        } else {
            (self.0[usize::from(byte / 8)] >> u32::from(byte % 8)) & 0b1 != 0
        }
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
        let block = &mut self.0[usize::from(byte / 8)];
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
    fn non_ascii() {
        for i in 0x80..=0xff {
            assert!(!HalfBytesMask::DFL_REST.contains(i));
        }
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

        assert_eq!(
            HalfBytesMask::from_bytes(
                b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+-._?="
            ),
            HalfBytesMask::DFL_REST,
        );
    }
}
