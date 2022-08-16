//! This module implements Nix language strings and their different
//! backing implementations.
use smol_str::SmolStr;
use std::hash::Hash;
use std::{borrow::Cow, fmt::Display};

#[derive(Clone, Debug)]
enum StringRepr {
    Smol(SmolStr),
    Heap(String),
}

#[repr(transparent)]
#[derive(Clone, Debug)]
pub struct NixString(StringRepr);

impl PartialEq for NixString {
    fn eq(&self, other: &Self) -> bool {
        self.as_str() == other.as_str()
    }
}

impl Eq for NixString {}

impl PartialOrd for NixString {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.as_str().partial_cmp(other.as_str())
    }
}

impl Ord for NixString {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.as_str().cmp(other.as_str())
    }
}

impl From<&str> for NixString {
    fn from(s: &str) -> Self {
        NixString(StringRepr::Smol(SmolStr::new(s)))
    }
}

impl From<String> for NixString {
    fn from(s: String) -> Self {
        NixString(StringRepr::Heap(s))
    }
}

impl Hash for NixString {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.as_str().hash(state)
    }
}

impl NixString {
    pub const NAME: Self = NixString(StringRepr::Smol(SmolStr::new_inline("name")));
    pub const VALUE: Self = NixString(StringRepr::Smol(SmolStr::new_inline("value")));

    pub fn as_str(&self) -> &str {
        match &self.0 {
            StringRepr::Smol(s) => s.as_str(),
            StringRepr::Heap(s) => s,
        }
    }

    // Return a displayable representation of the string as an
    // identifier.
    //
    // This is used when printing out strings used as e.g. attribute
    // set keys, as those are only escaped in the presence of special
    // characters.
    pub fn ident_str(&self) -> Cow<str> {
        let escaped = nix_escape_string(self.as_str());

        match escaped {
            // A borrowed string is unchanged and can be returned as
            // is.
            Cow::Borrowed(_) => escaped,

            // An owned string has escapes, and needs the outer quotes
            // for display.
            Cow::Owned(s) => Cow::Owned(format!("\"{}\"", s)),
        }
    }

    pub fn concat(&self, other: &Self) -> Self {
        let mut s = self.as_str().to_owned();
        s.push_str(other.as_str());
        NixString(StringRepr::Heap(s))
    }
}

fn nix_escape_char(ch: char) -> Option<&'static str> {
    match ch {
        '\\' => Some("\\\\"),
        '"' => Some("\\\""),
        '\n' => Some("\\n"),
        '\t' => Some("\\t"),
        '\r' => Some("\\r"),
        _ => None,
    }
}

// Escape a Nix string for display, as most user-visible representation
// are escaped strings.
//
// Note that this does not add the outer pair of surrounding quotes.
fn nix_escape_string(input: &str) -> Cow<str> {
    for (i, c) in input.chars().enumerate() {
        if let Some(esc) = nix_escape_char(c) {
            let mut escaped = String::with_capacity(input.len());
            escaped.push_str(&input[..i]);
            escaped.push_str(esc);

            for c in input[i + 1..].chars() {
                match nix_escape_char(c) {
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
        f.write_str(&nix_escape_string(self.as_str()))?;
        f.write_str("\"")
    }
}
