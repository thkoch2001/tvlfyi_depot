use data_encoding::BASE64;
use std::{
    fmt::{self, Display},
    mem,
};

use crate::nixbase32;

#[derive(Debug)]
pub struct NarInfo<'a> {
    // core (authenticated, but unverified here)
    pub store_path: StorePath<'a>,
    pub nar_hash: [u8; 32],
    pub nar_size: u64,
    pub references: Vec<StorePath<'a>>,
    // authenticity
    pub signatures: Vec<Signature<'a>>,
    pub ca: Option<ContentAddressed>,
    // bonus untrusted metadata
    pub system: Option<&'a str>,
    pub deriver: Option<StorePath<'a>>,
    // cache-specific untrusted metadata
    pub url: &'a str,
    pub compression: Option<&'a str>,
    pub file_hash: Option<[u8; 32]>,
    pub file_size: Option<u64>,
}

impl<'a> NarInfo<'a> {
    pub fn parse(input: &'a str) -> Option<Self> {
        let mut store_path = None;
        let mut url = None;
        let mut compression = None;
        let mut file_hash = None;
        let mut file_size = None;
        let mut nar_hash = None;
        let mut nar_size = None;
        let mut references = None;
        let mut system = None;
        let mut deriver = None;
        let mut signatures = vec![];
        let mut ca = None;

        for line in input.lines() {
            let (tag, val) = line.split_once(':')?;
            let val = val.strip_prefix(' ')?;

            match tag {
                "StorePath" => {
                    let val = val.strip_prefix("/nix/store/").and_then(StorePath::parse)?;

                    if store_path.replace(val).is_some() {
                        return None;
                    }
                }
                "URL" => {
                    if val.is_empty() {
                        return None;
                    }

                    if url.replace(val).is_some() {
                        return None;
                    }
                }
                "Compression" => {
                    if val.is_empty() {
                        return None;
                    }

                    if compression.replace(val).is_some() {
                        return None;
                    }
                }
                "FileHash" => {
                    let val = val.strip_prefix("sha256:")?;
                    let val = nixbase32::decode_fixed::<32>(val).ok()?;

                    if file_hash.replace(val).is_some() {
                        return None;
                    }
                }
                "FileSize" => {
                    let val = val.parse::<u64>().ok()?;

                    if file_size.replace(val).is_some() {
                        return None;
                    }
                }
                "NarHash" => {
                    let val = val.strip_prefix("sha256:")?;
                    let val = nixbase32::decode_fixed::<32>(val).ok()?;

                    if nar_hash.replace(val).is_some() {
                        return None;
                    }
                }
                "NarSize" => {
                    let val = val.parse::<u64>().ok()?;

                    if nar_size.replace(val).is_some() {
                        return None;
                    }
                }
                "References" => {
                    let val: Vec<StorePath> = if !val.is_empty() {
                        let mut prev = "";
                        val.split(' ')
                            .map(|s| {
                                if mem::replace(&mut prev, s) < s {
                                    StorePath::parse(s)
                                } else {
                                    // references are out of order
                                    None
                                }
                            })
                            .collect::<Option<_>>()?
                    } else {
                        vec![]
                    };

                    if references.replace(val).is_some() {
                        return None;
                    }
                }
                "System" => {
                    if val.is_empty() {
                        return None;
                    }

                    if system.replace(val).is_some() {
                        return None;
                    }
                }
                "Deriver" => {
                    let val = StorePath::parse(val)?;

                    if !val.name().ends_with(".drv") {
                        return None;
                    }

                    if deriver.replace(val).is_some() {
                        return None;
                    }
                }
                "Sig" => {
                    let val = Signature::parse(val)?;

                    signatures.push(val);
                }
                "CA" => {
                    let val = ContentAddressed::parse(val)?;

                    if ca.replace(val).is_some() {
                        return None;
                    }
                }
                _ => {
                    // unknown field, ignore
                }
            }
        }

        Some(NarInfo {
            store_path: store_path?,
            nar_hash: nar_hash?,
            nar_size: nar_size?,
            references: references?,
            signatures,
            ca,
            system,
            deriver,
            url: url?,
            compression,
            file_hash,
            file_size,
        })
    }
}

impl Display for NarInfo<'_> {
    fn fmt(&self, w: &mut fmt::Formatter) -> fmt::Result {
        writeln!(w, "StorePath: /nix/store/{}", self.store_path)?;
        writeln!(w, "URL: {}", self.url)?;

        if let Some(compression) = self.compression {
            writeln!(w, "Compression: {compression}")?;
        }

        if let Some(file_hash) = self.file_hash {
            writeln!(w, "FileHash: {}", Hash::Sha256(file_hash))?;
        }

        if let Some(file_size) = self.file_size {
            writeln!(w, "FileSize: {file_size}")?;
        }

        writeln!(w, "NarHash: {}", Hash::Sha256(self.nar_hash))?;
        writeln!(w, "NarSize: {}", self.nar_size)?;

        write!(w, "References:")?;
        if self.references.is_empty() {
            write!(w, " ")?;
        } else {
            for path in &self.references {
                write!(w, " {path}")?;
            }
        }
        writeln!(w)?;

        if let Some(deriver) = &self.deriver {
            writeln!(w, "Deriver: {deriver}")?;
        }

        if let Some(system) = self.system {
            writeln!(w, "System: {system}")?;
        }

        for sig in &self.signatures {
            writeln!(w, "Sig: {sig}")?;
        }

        if let Some(ca) = &self.ca {
            writeln!(w, "CA: {ca}")?;
        }

        Ok(())
    }
}

static PATH_CHARS: [bool; 256] = {
    let mut tbl = [false; 256];
    let mut c = 0;

    loop {
        tbl[c as usize] = match c {
            b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' => true,
            b'+' | b'-' | b'_' | b'?' | b'=' | b'.' => true,
            _ => false,
        };

        if c == u8::MAX {
            break;
        }

        c += 1;
    }

    tbl
};

fn valid_path_name(name: &str) -> bool {
    if name.is_empty() || name.len() > 211 || name.starts_with('.') {
        return false;
    }

    let mut valid = true;
    for c in name.bytes() {
        valid = valid && PATH_CHARS[c as usize];
    }

    valid
}

#[derive(PartialEq, Eq)]
pub struct StorePath<'a> {
    hash: [u8; 20],
    name: &'a str,
}

impl<'a> StorePath<'a> {
    pub fn parse(input: &'a str) -> Option<Self> {
        let (hash, name) = input.split_once('-')?;
        let hash = nixbase32::decode_fixed(hash).ok()?;

        if !valid_path_name(name) {
            return None;
        }

        Some(StorePath { hash, name })
    }

    pub fn hash(&self) -> &[u8; 20] {
        &self.hash
    }

    pub fn name(&self) -> &'a str {
        self.name
    }
}

impl fmt::Debug for StorePath<'_> {
    fn fmt(&self, w: &mut fmt::Formatter) -> fmt::Result {
        write!(w, "\"{}\"", self)
    }
}

impl fmt::Display for StorePath<'_> {
    fn fmt(&self, w: &mut fmt::Formatter) -> fmt::Result {
        write!(w, "{}-{}", nixbase32::encode(&self.hash), self.name)
    }
}

#[derive(Debug)]
pub struct Signature<'a> {
    name: &'a str,
    bytes: [u8; 64],
}

impl<'a> Signature<'a> {
    pub fn parse(input: &'a str) -> Option<Signature<'a>> {
        let (name, bytes64) = input.split_once(':')?;

        let mut buf = [0; 66];
        let mut bytes = [0; 64];
        match BASE64.decode_mut(bytes64.as_bytes(), &mut buf) {
            Ok(64) => {
                bytes.copy_from_slice(&buf[..64]);
            }
            _ => {
                return None;
            }
        }

        Some(Signature { name, bytes })
    }

    pub fn name(&self) -> &'a str {
        self.name
    }

    pub fn bytes(&self) -> &[u8; 64] {
        &self.bytes
    }
}

impl Display for Signature<'_> {
    fn fmt(&self, w: &mut fmt::Formatter) -> fmt::Result {
        write!(w, "{}:{}", self.name, BASE64.encode(&self.bytes))
    }
}

#[derive(Debug, Clone)]
pub enum ContentAddressed {
    Flat(Hash),
    Text([u8; 32]),
    Nar([u8; 32]),
}

impl ContentAddressed {
    pub fn parse(s: &str) -> Option<Self> {
        let (tag, s) = s.split_once(':')?;

        match tag {
            "text" => {
                let digest = s.strip_prefix("sha256:")?;
                let digest = nixbase32::decode_fixed(digest).ok()?;
                Some(ContentAddressed::Text(digest))
            }
            "fixed" => {
                if let Some(digest) = s.strip_prefix("r:sha256:") {
                    let digest = nixbase32::decode_fixed(digest).ok()?;
                    Some(ContentAddressed::Nar(digest))
                } else {
                    Hash::parse(s).map(ContentAddressed::Flat)
                }
            }
            _ => None,
        }
    }
}

impl Display for ContentAddressed {
    fn fmt(&self, w: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ContentAddressed::Flat(h) => {
                write!(w, "fixed:{h}")
            }
            &ContentAddressed::Text(d) => {
                write!(w, "text:{}", Hash::Sha256(d))
            }
            &ContentAddressed::Nar(d) => {
                write!(w, "fixed:r:{}", Hash::Sha256(d))
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Hash {
    Md5([u8; 16]),
    Sha1([u8; 20]),
    Sha256([u8; 32]),
    Sha512(Box<[u8; 64]>),
}

impl Hash {
    pub fn parse(s: &str) -> Option<Self> {
        let (tag, digest) = s.split_once(':')?;

        (match tag {
            "md5" => nixbase32::decode_fixed(digest).map(Hash::Md5),
            "sha1" => nixbase32::decode_fixed(digest).map(Hash::Sha1),
            "sha256" => nixbase32::decode_fixed(digest).map(Hash::Sha256),
            "sha512" => nixbase32::decode_fixed(digest)
                .map(Box::new)
                .map(Hash::Sha512),
            _ => return None,
        })
        .ok()
    }
}

impl Display for Hash {
    fn fmt(&self, w: &mut fmt::Formatter) -> fmt::Result {
        let (tag, digest) = match self {
            Hash::Md5(d) => ("md5", &d[..]),
            Hash::Sha1(d) => ("sha1", &d[..]),
            Hash::Sha256(d) => ("sha256", &d[..]),
            Hash::Sha512(d) => ("sha512", &d[..]),
        };

        write!(w, "{tag}:{}", nixbase32::encode(digest))
    }
}

#[cfg(test)]
mod test {
    use lazy_static::lazy_static;
    use pretty_assertions::assert_eq;
    use std::{io, str};

    use super::NarInfo;

    lazy_static! {
        static ref CASES: &'static [&'static str] = {
            let data = zstd::decode_all(io::Cursor::new(include_bytes!("../testdata/narinfo.zst")))
                .unwrap();
            let data = str::from_utf8(Vec::leak(data)).unwrap();
            Vec::leak(
                data.split_inclusive("\n\n")
                    .map(|s| s.strip_suffix('\n').unwrap())
                    .collect::<Vec<_>>(),
            )
        };
    }

    #[test]
    fn roundtrip() {
        for &input in *CASES {
            let parsed = NarInfo::parse(input).expect("should parse");
            let output = format!("{parsed}");
            assert_eq!(input, output, "should roundtrip");
        }
    }
}
