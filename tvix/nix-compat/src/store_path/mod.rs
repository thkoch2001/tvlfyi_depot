use crate::nixbase32;
use data_encoding::{DecodeError, BASE64};
use std::{
    fmt,
    path::PathBuf,
    str::{self, FromStr},
};
use thiserror;

#[cfg(target_family = "unix")]
use std::os::unix::ffi::OsStringExt;

mod utils;

pub use utils::*;

pub const DIGEST_SIZE: usize = 20;
pub const ENCODED_DIGEST_SIZE: usize = nixbase32::encode_len(DIGEST_SIZE);

// The store dir prefix, without trailing slash.
// That's usually where the Nix store is mounted at.
pub const STORE_DIR: &str = "/nix/store";
pub const STORE_DIR_WITH_SLASH: &str = "/nix/store/";

/// Errors that can occur when parsing a literal store path
#[derive(Debug, PartialEq, Eq, thiserror::Error)]
pub enum Error {
    #[error("Dash is missing between hash and name")]
    MissingDash,
    #[error("Hash encoding is invalid: {0}")]
    InvalidHashEncoding(DecodeError),
    #[error("Invalid length")]
    InvalidLength,
    #[error(
        "Invalid name: \"{}\", character at position {} is invalid",
        std::str::from_utf8(.0).unwrap_or(&BASE64.encode(.0)),
        .1,
    )]
    InvalidName(Vec<u8>, u8),
    #[error("Tried to parse an absolute path which was missing the store dir prefix.")]
    MissingStoreDir,
}

/// Represents a path in the Nix store (a direct child of [STORE_DIR]).
///
/// It consists of a digest (20 bytes), and a name, which is a string.
/// The name may only contain ASCII alphanumeric, or one of the following
/// characters: `-`, `_`, `.`, `+`, `?`, `=`.
/// The name is usually used to describe the pname and version of a package.
/// Derivation paths can also be represented as store paths, their names just
/// end with the `.drv` prefix.
///
/// A [StorePath] does not encode any additional subpath "inside" the store
/// path.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct StorePath {
    digest: [u8; DIGEST_SIZE],
    name: String,
}

impl StorePath {
    pub fn digest(&self) -> &[u8; DIGEST_SIZE] {
        &self.digest
    }

    pub fn name(&self) -> &str {
        self.name.as_ref()
    }
}

impl PartialOrd for StorePath {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.digest.partial_cmp(&other.digest)
    }
}

impl Ord for StorePath {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.digest.cmp(&other.digest)
    }
}

impl FromStr for StorePath {
    type Err = Error;

    /// Construct a [StorePath] by passing the `$digest-$name` string
    /// that comes after [STORE_DIR_WITH_SLASH].
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::from_bytes(s.as_bytes())
    }
}

impl StorePath {
    /// Construct a [StorePath] by passing the `$digest-$name` string
    /// that comes after [STORE_DIR_WITH_SLASH].
    pub fn from_bytes(s: &[u8]) -> Result<StorePath, Error> {
        Ok(StorePathRef::from_bytes(s)?.to_owned())
    }

    /// Construct a [StorePath] from an absolute store path string.
    /// This is equivalent to calling [StorePath::from_bytes], but stripping the
    /// [STORE_DIR_WITH_SLASH] prefix before.
    pub fn from_absolute_path(s: &[u8]) -> Result<StorePath, Error> {
        match s.strip_prefix(STORE_DIR_WITH_SLASH.as_bytes()) {
            Some(s_stripped) => Self::from_bytes(s_stripped),
            None => Err(Error::MissingStoreDir),
        }
    }

    /// Decompose a string into a [StorePath] and a [PathBuf] containing the
    /// rest of the path, or an error.
    #[cfg(target_family = "unix")]
    pub fn from_absolute_path_full(s: &str) -> Result<(StorePath, PathBuf), Error> {
        // strip [STORE_DIR_WITH_SLASH] from s
        match s.strip_prefix(STORE_DIR_WITH_SLASH) {
            None => Err(Error::MissingStoreDir),
            Some(rest) => {
                // put rest in a PathBuf
                let mut p = PathBuf::new();
                p.push(rest);

                let mut it = p.components();

                // The first component of the rest must be parse-able as a [StorePath]
                if let Some(first_component) = it.next() {
                    // convert first component to StorePath
                    let first_component_bytes = first_component.as_os_str().to_owned().into_vec();
                    let store_path = StorePath::from_bytes(&first_component_bytes)?;
                    // collect rest
                    let rest_buf: PathBuf = it.collect();
                    Ok((store_path, rest_buf))
                } else {
                    Err(Error::InvalidLength) // Well, or missing "/"?
                }
            }
        }
    }

    /// Converts the [StorePath] to an absolute store path string.
    /// That is just the string representation, prefixed with the store prefix
    /// ([STORE_DIR_WITH_SLASH]),
    pub fn to_absolute_path(&self) -> String {
        format!("{}{}", STORE_DIR_WITH_SLASH, self)
    }
}

/// Like [StorePath], but without a heap allocation for the name.
/// Used by [StorePath] for parsing.
///
/// TODO(edef): migrate most methods here
#[derive(Debug)]
pub struct StorePathRef<'a> {
    digest: [u8; DIGEST_SIZE],
    name: &'a str,
}

impl<'a> From<&'a StorePath> for StorePathRef<'a> {
    fn from(&StorePath { digest, ref name }: &'a StorePath) -> Self {
        StorePathRef {
            digest,
            name: name.as_ref(),
        }
    }
}

impl<'a> StorePathRef<'a> {
    pub fn digest(&self) -> &[u8; DIGEST_SIZE] {
        &self.digest
    }

    pub fn name(&self) -> &'a str {
        self.name
    }

    pub fn to_owned(&self) -> StorePath {
        StorePath {
            digest: self.digest,
            name: self.name.to_owned(),
        }
    }

    /// Construct a [StorePathRef] from a name and digest.
    pub fn from_name_and_digest(name: &'a str, digest: &[u8]) -> Result<StorePathRef<'a>, Error> {
        Ok(Self {
            name: validate_name(name.as_bytes())?,
            digest: digest.try_into().map_err(|_| Error::InvalidLength)?,
        })
    }

    /// Construct a [StorePathRef] by passing the `$digest-$name` string
    /// that comes after [STORE_DIR_WITH_SLASH].
    pub fn from_bytes(s: &'a [u8]) -> Result<Self, Error> {
        // the whole string needs to be at least:
        //
        // - 32 characters (encoded hash)
        // - 1 dash
        // - 1 character for the name
        if s.len() < ENCODED_DIGEST_SIZE + 2 {
            Err(Error::InvalidLength)?
        }

        let digest = nixbase32::decode_fixed(&s[..ENCODED_DIGEST_SIZE])
            .map_err(Error::InvalidHashEncoding)?;

        if s[ENCODED_DIGEST_SIZE] != b'-' {
            return Err(Error::MissingDash);
        }

        Ok(StorePathRef {
            digest,
            name: validate_name(&s[ENCODED_DIGEST_SIZE + 1..])?,
        })
    }
}

/// NAME_CHARS contains `true` for bytes that are valid in store path names,
/// not accounting for '.' being permitted only past the first character.
static NAME_CHARS: [bool; 256] = {
    let mut tbl = [false; 256];
    let mut c = 0;

    loop {
        tbl[c as usize] = matches!(c, b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'+' | b'-' | b'_' | b'?' | b'=' | b'.');

        if c == u8::MAX {
            break;
        }

        c += 1;
    }

    tbl
};

/// Checks a given &[u8] to match the restrictions for [StorePath::name], and
/// returns the name as string if successful.
pub(crate) fn validate_name(s: &(impl AsRef<[u8]> + ?Sized)) -> Result<&str, Error> {
    let s = s.as_ref();

    // Empty or excessively long names are not allowed.
    if s.is_empty() || s.len() > 211 {
        return Err(Error::InvalidLength);
    }

    if s[0] == b'.' {
        return Err(Error::InvalidName(s.to_vec(), 0));
    }

    let mut valid = true;
    for &c in s {
        valid = valid && NAME_CHARS[c as usize];
    }

    if !valid {
        for (i, &c) in s.iter().enumerate() {
            if !NAME_CHARS[c as usize] {
                return Err(Error::InvalidName(s.to_vec(), i as u8));
            }
        }

        unreachable!();
    }

    // SAFETY: We permit a subset of ASCII, which guarantees valid UTF-8.
    Ok(unsafe { str::from_utf8_unchecked(s) })
}

impl fmt::Display for StorePath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        StorePathRef::from(self).fmt(f)
    }
}

impl fmt::Display for StorePathRef<'_> {
    /// The string representation of a store path starts with a digest (20
    /// bytes), [crate::nixbase32]-encoded, followed by a `-`,
    /// and ends with the name.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}-{}", nixbase32::encode(&self.digest), self.name)
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use crate::store_path::DIGEST_SIZE;
    use hex_literal::hex;
    use test_case::test_case;

    use super::{Error, StorePath};

    #[test]
    fn happy_path() {
        let example_nix_path_str =
            "00bgd045z0d4icpbc2yyz4gx48ak44la-net-tools-1.60_p20170221182432";
        let nixpath = StorePath::from_bytes(example_nix_path_str.as_bytes())
            .expect("Error parsing example string");

        let expected_digest: [u8; DIGEST_SIZE] = hex!("8a12321522fd91efbd60ebb2481af88580f61600");

        assert_eq!("net-tools-1.60_p20170221182432", nixpath.name);
        assert_eq!(nixpath.digest, expected_digest);

        assert_eq!(example_nix_path_str, nixpath.to_string())
    }

    /// This is the store path rejected when `nix-store --add`'ing an
    /// empty `.gitignore` file.
    ///
    /// Nix 2.4 accidentally dropped this behaviour, but this is considered a bug.
    /// See https://github.com/NixOS/nix/pull/9095.
    #[test]
    fn starts_with_dot() {
        StorePath::from_bytes(b"fli4bwscgna7lpm7v5xgnjxrxh0yc7ra-.gitignore")
            .expect_err("must fail");
    }

    #[test]
    fn empty_name() {
        StorePath::from_bytes(b"00bgd045z0d4icpbc2yy-").expect_err("must fail");
    }

    #[test]
    fn excessive_length() {
        StorePath::from_bytes(b"00bgd045z0d4icpbc2yy-aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
            .expect_err("must fail");
    }

    #[test]
    fn invalid_hash_length() {
        StorePath::from_bytes(b"00bgd045z0d4icpbc2yy-net-tools-1.60_p20170221182432")
            .expect_err("must fail");
    }

    #[test]
    fn invalid_encoding_hash() {
        StorePath::from_bytes(b"00bgd045z0d4icpbc2yyz4gx48aku4la-net-tools-1.60_p20170221182432")
            .expect_err("must fail");
    }

    #[test]
    fn more_than_just_the_bare_nix_store_path() {
        StorePath::from_bytes(
            b"00bgd045z0d4icpbc2yyz4gx48aku4la-net-tools-1.60_p20170221182432/bin/arp",
        )
        .expect_err("must fail");
    }

    #[test]
    fn no_dash_between_hash_and_name() {
        StorePath::from_bytes(b"00bgd045z0d4icpbc2yyz4gx48ak44lanet-tools-1.60_p20170221182432")
            .expect_err("must fail");
    }

    #[test]
    fn absolute_path() {
        let example_nix_path_str =
            "00bgd045z0d4icpbc2yyz4gx48ak44la-net-tools-1.60_p20170221182432";
        let nixpath_expected =
            StorePath::from_bytes(example_nix_path_str.as_bytes()).expect("must parse");

        let nixpath_actual = StorePath::from_absolute_path(
            "/nix/store/00bgd045z0d4icpbc2yyz4gx48ak44la-net-tools-1.60_p20170221182432".as_bytes(),
        )
        .expect("must parse");

        assert_eq!(nixpath_expected, nixpath_actual);

        assert_eq!(
            "/nix/store/00bgd045z0d4icpbc2yyz4gx48ak44la-net-tools-1.60_p20170221182432",
            nixpath_actual.to_absolute_path(),
        );
    }

    #[test]
    fn absolute_path_missing_prefix() {
        assert_eq!(
            Error::MissingStoreDir,
            StorePath::from_absolute_path(b"foobar-123").expect_err("must fail")
        );
    }

    #[test_case(
        "/nix/store/00bgd045z0d4icpbc2yyz4gx48ak44la-net-tools-1.60_p20170221182432",
        (StorePath::from_bytes(b"00bgd045z0d4icpbc2yyz4gx48ak44la-net-tools-1.60_p20170221182432").unwrap(), PathBuf::new())
    ; "without prefix")]
    #[test_case(
        "/nix/store/00bgd045z0d4icpbc2yyz4gx48ak44la-net-tools-1.60_p20170221182432/",
        (StorePath::from_bytes(b"00bgd045z0d4icpbc2yyz4gx48ak44la-net-tools-1.60_p20170221182432").unwrap(), PathBuf::new())
    ; "without prefix, but trailing slash")]
    #[test_case(
        "/nix/store/00bgd045z0d4icpbc2yyz4gx48ak44la-net-tools-1.60_p20170221182432/bin/arp",
        (StorePath::from_bytes(b"00bgd045z0d4icpbc2yyz4gx48ak44la-net-tools-1.60_p20170221182432").unwrap(), PathBuf::from("bin/arp"))
    ; "with prefix")]
    #[test_case(
        "/nix/store/00bgd045z0d4icpbc2yyz4gx48ak44la-net-tools-1.60_p20170221182432/bin/arp/",
        (StorePath::from_bytes(b"00bgd045z0d4icpbc2yyz4gx48ak44la-net-tools-1.60_p20170221182432").unwrap(), PathBuf::from("bin/arp/"))
    ; "with prefix and trailing slash")]
    fn from_absolute_path_full(s: &str, expected: (StorePath, PathBuf)) {
        let actual = StorePath::from_absolute_path_full(s).expect("must succeed");
        assert_eq!(expected, actual);
    }

    #[test]
    fn from_absolute_path_errors() {
        assert_eq!(
            Error::InvalidLength,
            StorePath::from_absolute_path_full("/nix/store/").expect_err("must fail")
        );
        assert_eq!(
            Error::InvalidLength,
            StorePath::from_absolute_path_full("/nix/store/foo").expect_err("must fail")
        );
        assert_eq!(
            Error::MissingStoreDir,
            StorePath::from_absolute_path_full(
                "00bgd045z0d4icpbc2yyz4gx48ak44la-net-tools-1.60_p20170221182432"
            )
            .expect_err("must fail")
        );
    }
}
