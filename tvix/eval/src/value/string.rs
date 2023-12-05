//! This module implements Nix language strings.
//!
//! Nix language strings never need to be modified on the language
//! level, allowing us to shave off some memory overhead and only
//! paying the cost when creating new strings.
use bstr::{BStr, BString, ByteSlice, ByteVec, Chars};
use rnix::ast;
use std::borrow::Cow;
use std::ffi::OsStr;
use std::fmt::Display;
use std::hash::Hash;
use std::ops::Deref;
use std::path::Path;

use serde::de::{Deserializer, Visitor};
use serde::{Deserialize, Serialize};

#[repr(transparent)]
#[derive(Clone, Debug, Serialize, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NixString(BString);

impl PartialEq<&[u8]> for NixString {
    fn eq(&self, other: &&[u8]) -> bool {
        **self == **other
    }
}

impl From<&BStr> for NixString {
    fn from(value: &BStr) -> Self {
        Self(value.to_owned())
    }
}

impl From<&[u8]> for NixString {
    fn from(value: &[u8]) -> Self {
        Self(value.into())
    }
}

impl From<Vec<u8>> for NixString {
    fn from(value: Vec<u8>) -> Self {
        Self(value.into())
    }
}

impl From<&str> for NixString {
    fn from(s: &str) -> Self {
        Self::from(s.as_bytes())
    }
}

impl From<String> for NixString {
    fn from(s: String) -> Self {
        Self(s.into())
    }
}

impl From<Box<str>> for NixString {
    fn from(s: Box<str>) -> Self {
        Self(s.into_boxed_bytes().into_vec().into())
    }
}

impl From<BString> for NixString {
    fn from(s: BString) -> Self {
        Self(s)
    }
}

impl From<ast::Ident> for NixString {
    fn from(ident: ast::Ident) -> Self {
        ident.ident_token().unwrap().text().into()
    }
}

impl<'a> From<&'a NixString> for &'a BStr {
    fn from(s: &'a NixString) -> Self {
        BStr::new(&*s.0)
    }
}

impl AsRef<[u8]> for NixString {
    fn as_ref(&self) -> &[u8] {
        &**self.0
    }
}

impl<'de> Deserialize<'de> for NixString {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct StringVisitor;

        impl<'de> Visitor<'de> for StringVisitor {
            type Value = NixString;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("a valid Nix string")
            }

            fn visit_string<E>(self, v: String) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(v.into())
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(v.into())
            }
        }

        deserializer.deserialize_string(StringVisitor)
    }
}

#[cfg(feature = "arbitrary")]
mod arbitrary {
    use super::*;
    use proptest::prelude::{any_with, Arbitrary};
    use proptest::strategy::{BoxedStrategy, Strategy};

    impl Arbitrary for NixString {
        type Parameters = <String as Arbitrary>::Parameters;

        type Strategy = BoxedStrategy<Self>;

        fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
            any_with::<String>(args).prop_map(Self::from).boxed()
        }
    }
}

impl NixString {
    pub fn as_bstr(&self) -> &BStr {
        BStr::new(self.as_bytes())
    }

    pub fn as_bytes(&self) -> &[u8] {
        &self.0
    }

    /// Return a displayable representation of the string as an
    /// identifier.
    ///
    /// This is used when printing out strings used as e.g. attribute
    /// set keys, as those are only escaped in the presence of special
    /// characters.
    pub fn ident_str(&self) -> Cow<str> {
        let escaped = nix_escape_string(&self.to_str_lossy());

        match escaped {
            // A borrowed string is unchanged and can be returned as
            // is.
            Cow::Borrowed(_) => {
                if is_valid_nix_identifier(&escaped) && !is_keyword(&escaped) {
                    escaped
                } else {
                    Cow::Owned(format!("\"{}\"", escaped))
                }
            }

            // An owned string has escapes, and needs the outer quotes
            // for display.
            Cow::Owned(s) => Cow::Owned(format!("\"{}\"", s)),
        }
    }

    pub fn concat(&self, other: &Self) -> Self {
        let mut s = self.0.clone();
        s.extend(other.0.bytes());
        Self(s)
    }

    pub fn chars(&self) -> Chars<'_> {
        self.0.chars()
    }
}

fn nix_escape_char(ch: char, next: Option<&char>) -> Option<&'static str> {
    match (ch, next) {
        ('\\', _) => Some("\\\\"),
        ('"', _) => Some("\\\""),
        ('\n', _) => Some("\\n"),
        ('\t', _) => Some("\\t"),
        ('\r', _) => Some("\\r"),
        ('$', Some('{')) => Some("\\$"),
        _ => None,
    }
}

/// Return true if this string is a keyword -- character strings
/// which lexically match the "identifier" production but are not
/// parsed as identifiers.  See also cppnix commit
/// b72bc4a972fe568744d98b89d63adcd504cb586c.
fn is_keyword(s: &str) -> bool {
    matches!(
        s,
        "if" | "then" | "else" | "assert" | "with" | "let" | "in" | "rec" | "inherit"
    )
}

/// Return true if this string can be used as an identifier in Nix.
fn is_valid_nix_identifier(s: &str) -> bool {
    // adapted from rnix-parser's tokenizer.rs
    let mut chars = s.chars();
    match chars.next() {
        Some('a'..='z' | 'A'..='Z' | '_') => (),
        _ => return false,
    }
    for c in chars {
        match c {
            'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-' | '\'' => (),
            _ => return false,
        }
    }
    true
}

/// Escape a Nix string for display, as most user-visible representation
/// are escaped strings.
///
/// Note that this does not add the outer pair of surrounding quotes.
fn nix_escape_string(input: &str) -> Cow<str> {
    let mut iter = input.char_indices().peekable();

    while let Some((i, c)) = iter.next() {
        if let Some(esc) = nix_escape_char(c, iter.peek().map(|(_, c)| c)) {
            let mut escaped = String::with_capacity(input.len());
            escaped.push_str(&input[..i]);
            escaped.push_str(esc);

            // In theory we calculate how many bytes it takes to represent `esc`
            // in UTF-8 and use that for the offset. It is, however, safe to
            // assume that to be 1, as all characters that can be escaped in a
            // Nix string are ASCII.
            let mut inner_iter = input[i + 1..].chars().peekable();
            while let Some(c) = inner_iter.next() {
                match nix_escape_char(c, inner_iter.peek()) {
                    Some(esc) => escaped.push_str(esc),
                    None => escaped.push(c),
                }
            }

            return Cow::Owned(escaped);
        }
    }

    Cow::Borrowed(input)
}

impl Display for NixString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("\"")?;
        f.write_str(&nix_escape_string(&self.to_str_lossy()))?;
        f.write_str("\"")
    }
}

impl Deref for NixString {
    type Target = Vec<u8>;

    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::properties::{eq_laws, hash_laws, ord_laws};

    eq_laws!(NixString);
    hash_laws!(NixString);
    ord_laws!(NixString);
}
