use nix_compat::{
    narinfo::{Flags, Signature},
    nixhash::CAHash,
    store_path::StorePath,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PathInfo {
    pub store_path: StorePath<String>,
    /// The path can be a directory, file or symlink.
    pub node: tvix_castore::Node,
    /// A list of references. To validate .narinfo signatures, a fingerprint needs
    /// to be constructed.
    /// This fingerprint doesn't just contain the hashes of the output paths of all
    /// references (like PathInfo.references), but their whole (base)names, so we
    /// need to keep them somewhere.
    pub references: Vec<StorePath<String>>,

    pub nar_size: u64,
    /// The sha256 of the NAR file representation.
    pub nar_sha256: [u8; 32],
    /// The signatures in a .narinfo file.
    pub signatures: Vec<Signature<String>>,
    /// The StorePath of the .drv file producing this output.
    /// The .drv suffix is omitted in its `name` field.
    pub deriver: Option<StorePath<String>>,
    /// The CA field in the .narinfo.
    /// Its textual representations seen in the wild are one of the following:
    ///
    /// * `fixed:r:sha256:1gcky5hlf5vqfzpyhihydmm54grhc94mcs8w7xr8613qsqb1v2j6`
    ///   fixed-output derivations using "recursive" `outputHashMode`.
    /// * `fixed:sha256:19xqkh72crbcba7flwxyi3n293vav6d7qkzkh2v4zfyi4iia8vj8 fixed-output derivations using "flat" `outputHashMode\`
    /// * `text:sha256:19xqkh72crbcba7flwxyi3n293vav6d7qkzkh2v4zfyi4iia8vj8`
    ///   Text hashing, used for uploaded .drv files and outputs produced by
    ///   builtins.toFile.
    ///
    /// Semantically, they can be split into the following components:
    ///
    /// * "content address prefix". Currently, "fixed" and "text" are supported.
    /// * "hash mode". Currently, "flat" and "recursive" are supported.
    /// * "hash type". The underlying hash function used.
    ///   Currently, sha1, md5, sha256, sha512.
    /// * "digest". The digest itself.
    ///
    /// There are some restrictions on the possible combinations.
    /// For example, `text` and `fixed:recursive` always imply sha256.
    ///
    /// We use an enum to encode the possible combinations, and optimize for the
    /// common case, `fixed:recursive`, identified as `NAR_SHA256`.
    pub ca: Option<CAHash>,
}

impl PathInfo {
    /// With self and its store path name, this reconstructs a
    /// [nix_compat::narinfo::NarInfo<'_>].
    /// It can be used to validate Signatures, or get back a (sparse) NarInfo
    /// struct to prepare writing it out.
    ///
    /// It assumes self to be validated first, and will only return None if the
    /// `narinfo` field is unpopulated.
    ///
    /// It does very little allocation (a Vec each for `signatures` and
    /// `references`), the rest points to data owned elsewhere.
    ///
    /// Keep in mind this is not able to reconstruct all data present in the
    /// NarInfo<'_>, as some of it is not stored at all:
    /// - the `system`, `file_hash` and `file_size` fields are set to `None`.
    /// - the URL is set to an empty string.
    /// - Compression is set to "none"
    ///
    /// If you want to render it out to a string and be able to parse it back
    /// in, at least URL *must* be set again.
    pub fn to_narinfo(&self) -> nix_compat::narinfo::NarInfo<'_> {
        nix_compat::narinfo::NarInfo {
            flags: Flags::empty(),
            store_path: self.store_path.as_ref(),
            nar_hash: self.nar_sha256,
            nar_size: self.nar_size,
            references: self.references.iter().map(StorePath::as_ref).collect(),
            signatures: self.signatures.iter().map(Signature::as_ref).collect(),
            ca: self.ca.clone(),
            system: None,
            deriver: self.deriver.as_ref().map(StorePath::as_ref),
            url: "",
            compression: Some("none"),
            file_hash: None,
            file_size: None,
        }
    }
}
