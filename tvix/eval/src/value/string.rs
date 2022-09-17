//! This module implements Nix language strings and their different
//! backing implementations.
use smol_str::SmolStr;
use std::hash::Hash;
use std::ops::RangeInclusive;
use std::{borrow::Cow, fmt::Display, str::Chars};

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

impl From<SmolStr> for NixString {
    fn from(s: SmolStr) -> Self {
        NixString(StringRepr::Smol(s))
    }
}

impl Hash for NixString {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.as_str().hash(state)
    }
}

impl NixString {
    pub const NAME: Self = NixString(StringRepr::Smol(SmolStr::new_inline("name")));
    pub const NAME_REF: &'static Self = &Self::NAME;

    pub const VALUE: Self = NixString(StringRepr::Smol(SmolStr::new_inline("value")));
    pub const VALUE_REF: &'static Self = &Self::VALUE;

    pub fn as_str(&self) -> &str {
        match &self.0 {
            StringRepr::Smol(s) => s.as_str(),
            StringRepr::Heap(s) => s,
        }
    }

    /// Return a displayable representation of the string as an
    /// identifier.
    ///
    /// This is used when printing out strings used as e.g. attribute
    /// set keys, as those are only escaped in the presence of special
    /// characters.
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

    pub fn chars(&self) -> Chars<'_> {
        match &self.0 {
            StringRepr::Heap(h) => h.chars(),
            StringRepr::Smol(s) => s.chars(),
        }
    }

    /// Assumes the string is a version number and returns an iterator
    /// over the parts
    ///
    /// This can then be directly used to compare two versions
    pub fn version_parts(&self) -> VersionPartsIter {
        VersionPartsIter {
            cached_part: InternalPart::Break,
            iter: self.as_str().char_indices(),
            version: self.as_str(),
        }
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

/// Escape a Nix string for display, as most user-visible representation
/// are escaped strings.
///
/// Note that this does not add the outer pair of surrounding quotes.
fn nix_escape_string(input: &str) -> Cow<str> {
    let mut iter = input.chars().enumerate().peekable();

    while let Some((i, c)) = iter.next() {
        if let Some(esc) = nix_escape_char(c, iter.peek().map(|(_, c)| c)) {
            let mut escaped = String::with_capacity(input.len());
            escaped.push_str(&input[..i]);
            escaped.push_str(esc);

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
        f.write_str(&nix_escape_string(self.as_str()))?;
        f.write_str("\"")
    }
}

/// Version strings can be broken up into Parts.
/// One Part represents either a string of digits or characters.
/// '.' and '_' represent deviders between parts and are not included in any part.
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug)]
pub enum VersionPart<'a> {
    Word(&'a str),
    Number(u64),
}

/// Type used to hold information about a VersionPart during creation
enum InternalPart {
    Number { range: RangeInclusive<usize> },
    Word { range: RangeInclusive<usize> },
    Break,
}

/// An iterator which yields the parts of a version string.
///
/// This `struct` is created by the [`version_parts`] method on [`NixString`]. See
/// its documentation for more.
///
/// [`step_by`]: NixString::version_parts
/// [`NixString`]: struct.NixString.html
pub struct VersionPartsIter<'a> {
    cached_part: InternalPart,
    iter: std::str::CharIndices<'a>,
    version: &'a str,
}

impl<'a> Iterator for VersionPartsIter<'a> {
    type Item = VersionPart<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let char = self.iter.next();

        if char.is_none() {
            let mut cached_part = InternalPart::Break;
            std::mem::swap(&mut cached_part, &mut self.cached_part);
            match cached_part {
                InternalPart::Break => return None,
                InternalPart::Number { range } => {
                    return Some(VersionPart::Number(self.version[range].parse().unwrap()))
                }
                InternalPart::Word { range } => {
                    return Some(VersionPart::Word(&self.version[range]))
                }
            }
        }

        let (pos, char) = char.unwrap();
        match char {
            // Divider encountered
            '.' | '_' => {
                let mut cached_part = InternalPart::Break;
                std::mem::swap(&mut cached_part, &mut self.cached_part);
                match cached_part {
                    InternalPart::Number { range } => {
                        Some(VersionPart::Number(self.version[range].parse().unwrap()))
                    }
                    InternalPart::Word { range } => Some(VersionPart::Word(&self.version[range])),
                    InternalPart::Break => self.next(),
                }
            }

            // digit encountered
            _ if char.is_ascii_digit() => {
                let mut cached_part = InternalPart::Number { range: pos..=pos };
                std::mem::swap(&mut cached_part, &mut self.cached_part);
                match cached_part {
                    InternalPart::Number { range } => {
                        self.cached_part = InternalPart::Number {
                            range: *range.start()..=*range.end() + 1,
                        };
                        self.next()
                    }
                    InternalPart::Word { range } => Some(VersionPart::Word(&self.version[range])),
                    InternalPart::Break => self.next(),
                }
            }

            // char encountered
            _ => {
                let mut cached_part = InternalPart::Word { range: pos..=pos };
                std::mem::swap(&mut cached_part, &mut self.cached_part);
                match cached_part {
                    InternalPart::Word { range } => {
                        self.cached_part = InternalPart::Word {
                            range: *range.start()..=*range.end() + char.len_utf8(),
                        };
                        self.next()
                    }
                    InternalPart::Number { range } => {
                        Some(VersionPart::Number(self.version[range].parse().unwrap()))
                    }
                    InternalPart::Break => self.next(),
                }
            }
        }
    }
}
