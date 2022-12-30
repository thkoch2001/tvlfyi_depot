use crate::nixbase32::NIXBASE32;
use data_encoding::{DecodeError, DecodeKind};

const PATH_HASH_SIZE: usize = 20;

pub struct NixPath {
    name: String,
    digest: Vec<u8>,
}

impl NixPath {
    fn validate_characters(s: &str) -> Result<(), DecodeError> {
        for (ii, c) in s.char_indices() {
            if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') {
                continue;
            }

            if c == '-' || c == '_' || c == '.' || c == '+' || c == '?' || c == '=' {
                continue;
            }

            return Err(DecodeError {
                position: ii,
                kind: DecodeKind::Symbol,
            });
        }
        return Ok(());
    }

    pub fn from_string(s: &str) -> Result<NixPath, DecodeError> {
        let encoded_path_hash_size: usize = NIXBASE32.encode_len(PATH_HASH_SIZE);
        let name_offset = encoded_path_hash_size + 1;

        if s.len() < name_offset + 1 {
            return Err(DecodeError {
                position: s.len(),
                kind: DecodeKind::Length,
            });
        }

        let digest = NIXBASE32.decode(s[..encoded_path_hash_size].as_bytes())?;

        NixPath::validate_characters(&s[name_offset..])?;

        return Ok(NixPath {
            name: s[name_offset..].to_string(),
            digest: digest,
        });
    }

    pub fn to_string(&self) -> String {
        return format!(
            "{}-{}",
            crate::nixbase32::NIXBASE32.encode(&self.digest),
            self.name
        );
    }
}

#[cfg(test)]
mod tests {
    use super::NixPath;

    #[test]
    fn happy_path() {
        let example_nix_path_str =
            "00bgd045z0d4icpbc2yyz4gx48ak44la-net-tools-1.60_p20170221182432";
        let nixpath =
            NixPath::from_string(&example_nix_path_str).expect("Error parsing example string");

        assert_eq!("net-tools-1.60_p20170221182432", nixpath.name);
        assert_eq!(
            nixpath.digest,
            &[
                0x8a, 0x12, 0x32, 0x15, 0x22, 0xfd, 0x91, 0xef, 0xbd, 0x60, 0xeb, 0xb2, 0x48, 0x1a,
                0xf8, 0x85, 0x80, 0xf6, 0x16, 0x00,
            ]
        );

        assert_eq!(example_nix_path_str, nixpath.to_string())
    }

    #[test]
    fn invalid_hash_length() {
        let s = "00bgd045z0d4icpbc2yy-net-tools-1.60_p20170221182432";

        let test_passed = match NixPath::from_string(s) {
            Ok(_) => false,
            Err(_) => true,
        };

        assert!(test_passed);
    }

    #[test]
    fn invalid_encoding_hash() {
        let s = "00bgd045z0d4icpbc2yyz4gx48aku4la-net-tools-1.60_p20170221182432";

        let test_passed = match NixPath::from_string(s) {
            Ok(_) => false,
            Err(_) => true,
        };

        assert!(test_passed);
    }

    #[test]
    fn more_than_just_the_bare_nix_store_path() {
        let s = "00bgd045z0d4icpbc2yyz4gx48aku4la-net-tools-1.60_p20170221182432/bin/arp";

        let test_passed = match NixPath::from_string(s) {
            Ok(_) => false,
            Err(_) => true,
        };

        assert!(test_passed);
    }
}
