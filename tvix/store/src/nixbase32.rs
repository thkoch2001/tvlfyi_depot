use data_encoding::{DecodeError, Specification};
use lazy_static::lazy_static;

/// Nixbase32Encoding wraps a data_encoding::Encoding internally.
/// We can't use it directly, as nix also reads in characters in reverse order.
pub struct Nixbase32Encoding {
    encoding: data_encoding::Encoding,
}

lazy_static! {
    pub static ref NIXBASE32: Nixbase32Encoding = nixbase32_encoding();
}

impl Nixbase32Encoding {
    pub fn encode(&self, input: &[u8]) -> String {
        // Reverse the input, reading in the bytes in reverse order.
        let mut reversed = Vec::with_capacity(input.len());
        reversed.extend(input.iter().rev());
        self.encoding.encode(&reversed)
    }

    pub fn decode(&self, input: &[u8]) -> Result<Vec<u8>, DecodeError> {
        // Decode first, then reverse the bytes of the output.
        let output = self.encoding.decode(input)?;

        let mut reversed = Vec::with_capacity(output.len());
        reversed.extend(output.iter().rev());
        Ok(reversed)
    }

    /// Returns the decoded length of an input of length len.
    pub fn decode_len(&self, len: usize) -> Result<usize, DecodeError> {
        self.encoding.decode_len(len)
    }

    /// Returns the encoded length of an input of length len
    pub fn encode_len(&self, len: usize) -> usize {
        self.encoding.encode_len(len)
    }
}

fn nixbase32_encoding() -> Nixbase32Encoding {
    let mut spec = Specification::new();
    spec.symbols.push_str("0123456789abcdfghijklmnpqrsvwxyz");

    Nixbase32Encoding {
        encoding: spec.encoding().unwrap(),
    }
}

#[cfg(test)]
mod tests {
    use crate::nixbase32::NIXBASE32;

    #[test]
    fn decode() {
        let dec = NIXBASE32
            .decode("00bgd045z0d4icpbc2yyz4gx48ak44la".as_bytes())
            .unwrap();

        assert_eq!(
            dec,
            [
                0x8a, 0x12, 0x32, 0x15, 0x22, 0xfd, 0x91, 0xef, 0xbd, 0x60, 0xeb, 0xb2, 0x48, 0x1a,
                0xf8, 0x85, 0x80, 0xf6, 0x16, 0x00,
            ]
        )
    }

    #[test]
    fn encode_len() {
        assert_eq!(NIXBASE32.encode_len(20), 32)
    }

    #[test]
    fn decode_len() {
        assert_eq!(NIXBASE32.decode_len(32).unwrap(), 20)
    }

    #[test]
    fn encode() {
        let enc = NIXBASE32.encode(
            &[
                0x8a, 0x12, 0x32, 0x15, 0x22, 0xfd, 0x91, 0xef, 0xbd, 0x60, 0xeb, 0xb2, 0x48, 0x1a,
                0xf8, 0x85, 0x80, 0xf6, 0x16, 0x00,
            ][..],
        );

        assert_eq!(enc, "00bgd045z0d4icpbc2yyz4gx48ak44la");
    }
}
