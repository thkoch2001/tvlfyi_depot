//! Implements the slightly odd "base32" encoding that's used in Nix.
//!
//! Nix uses a custom alphabet. Contrary to other implementations (RFC4648),
//! encoding to "nix base32" doesn't use any padding, and reads in characters
//! in reverse order.
//!
//! This is also the main reason why we can't use `data_encoding::Encoding` -
//! it gets things wrong if there normally would be a need for padding.

use lazy_static::lazy_static;
use thiserror::Error;

pub struct Nixbase32Encoding {}
const ALPHABET: &'static [char; 32] = &[
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'f', 'g', 'h', 'i', 'j',
    'k', 'l', 'm', 'n', 'p', 'q', 'r', 's', 'v', 'w', 'x', 'y', 'z',
];

lazy_static! {
    /// Returns a Nixbase32Encoding providing some functions seen on a data_encoding::Encoding.
    pub static ref NIXBASE32: Nixbase32Encoding = Nixbase32Encoding{};
}

/// Errors that can occur while decoding nixbase32-encoded data.
#[derive(Debug, Eq, PartialEq, Error)]
pub enum Nixbase32DecodeError {
    #[error("character {0} not in alphabet")]
    CharacterNotInAlphabet(char),
    #[error("nonzero carry")]
    NonzeroCarry(),
}

impl Nixbase32Encoding {
    /// Returns encoded input
    pub fn encode(&self, input: &[u8]) -> String {
        let output_len = self.encode_len(input.len());
        let mut output = String::with_capacity(output_len);

        if output_len > 0 {
            for n in (0..=output_len - 1).rev() {
                let b = n * 5;
                let i = b / 8;
                let j = b % 8;

                let mut c = input[i] >> j;
                if i + 1 < input.len() {
                    // we want to right shift, and discard shifted out bits (unchecked)
                    // To do this without panicing, we need to do the shifting in u16
                    // and convert back to u8 afterwards.
                    c |= ((input[i + 1] as u16) << 8 - j as u16) as u8
                }

                output.push(ALPHABET[(c & 0x1f) as usize]);
            }
        }

        output
    }

    /// Returns decoded input
    pub fn decode(&self, input: &[u8]) -> Result<Vec<u8>, Nixbase32DecodeError> {
        let output_len = self.decode_len(input.len());
        let mut output: Vec<u8> = vec![0x00; output_len];

        for n in 0..input.len() {
            let c = input[input.len() - 1 - n];

            // find character in alphabet
            match ALPHABET.iter().position(|e| e == &(c as char)) {
                None => return Err(Nixbase32DecodeError::CharacterNotInAlphabet(c as char)),
                Some(pos) => {
                    let b = n * 5;
                    let i = b / 8;
                    let j = b % 8;

                    // OR the main pattern
                    output[i] |= ((pos as u16) << (j as u16)) as u8;

                    // calculate the "carry pattern"
                    // This is `pos >> (8-j)`, but we need to be explicit we
                    // *want* to discard the overflow.
                    let carry: u8 = (((pos as u16) << 8 as u16) >> (8 - j as u16)) as u8;

                    // if we're at the end of dst…
                    if i == output_len - 1 {
                        // but have a nonzero carry, the encoding is invalid.
                        if carry != 0 {
                            return Err(Nixbase32DecodeError::NonzeroCarry());
                        }
                    } else {
                        output[i + 1] |= carry;
                    }
                }
            }
        }

        Ok(output)
    }

    /// Returns the decoded length of an input of length len.
    pub fn decode_len(&self, len: usize) -> usize {
        return (len * 5) / 8;
    }

    /// Returns the encoded length of an input of length len
    pub fn encode_len(&self, len: usize) -> usize {
        if len == 0 {
            return 0;
        }
        return (len * 8 - 1) / 5 + 1;
    }
}

#[cfg(test)]
mod tests {
    use crate::nixbase32::NIXBASE32;
    use test_case::test_case;

    #[test_case("", vec![] ; "empty bytes")]
    #[test_case("0z", vec![0x1f]; "one byte")]
    #[test_case("00bgd045z0d4icpbc2yyz4gx48ak44la", vec![
                 0x8a, 0x12, 0x32, 0x15, 0x22, 0xfd, 0x91, 0xef, 0xbd, 0x60, 0xeb, 0xb2, 0x48, 0x1a,
                 0xf8, 0x85, 0x80, 0xf6, 0x16, 0x00]; "store path")]
    #[test_case("0c5b8vw40dy178xlpddw65q9gf1h2186jcc3p4swinwggbllv8mk", vec![
        0xb3, 0xa2, 0x4d, 0xe9, 0x7a, 0x8f, 0xdb, 0xc8, 0x35, 0xb9, 0x83, 0x31, 0x69, 0x50, 0x10, 0x30,
        0xb8, 0x97, 0x70, 0x31, 0xbc, 0xb5, 0x4b, 0x3b, 0x3a, 0xc1, 0x37, 0x40, 0xf8, 0x46, 0xab, 0x30,
    ]; "sha256")]
    fn encode(enc: &str, dec: Vec<u8>) {
        assert_eq!(enc, NIXBASE32.encode(&dec));
    }

    #[test_case("", Some(vec![]) ; "empty bytes")]
    #[test_case("0z", Some(vec![0x1f]); "one byte")]
    #[test_case("00bgd045z0d4icpbc2yyz4gx48ak44la", Some(vec![
                 0x8a, 0x12, 0x32, 0x15, 0x22, 0xfd, 0x91, 0xef, 0xbd, 0x60, 0xeb, 0xb2, 0x48, 0x1a,
                 0xf8, 0x85, 0x80, 0xf6, 0x16, 0x00]); "store path")]
    // this is invalid encoding, because it encodes 10 1-bytes, so the carry
    // would be 2 1-bytes
    #[test_case("zz", None; "invalid encoding-1")]
    // this is an even more specific example - it'd decode as 00000000 11
    #[test_case("c0", None; "invalid encoding-2")]

    fn decode(enc: &str, dec: Option<Vec<u8>>) {
        match dec {
            Some(dec) => {
                // The decode needs to match what's passed in dec
                assert_eq!(dec, NIXBASE32.decode(enc.as_bytes()).unwrap());
            }
            None => {
                // the decode needs to be an error
                assert_eq!(true, NIXBASE32.decode(enc.as_bytes()).is_err());
            }
        }
    }

    #[test]
    fn encode_len() {
        assert_eq!(NIXBASE32.encode_len(20), 32)
    }

    #[test]
    fn decode_len() {
        assert_eq!(NIXBASE32.decode_len(32), 20)
    }
}
