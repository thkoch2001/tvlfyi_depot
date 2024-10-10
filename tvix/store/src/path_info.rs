use crate::proto::ValidatePathInfoError;
use bytes::Bytes;

use crate::proto;
use tvix_castore::proto as castorepb;

use nix_compat::{
    narinfo::{Signature, SignatureError},
    nixhash::CAHash,
    store_path::{self, StorePath, StorePathRef},
};

struct PathInfo2 {
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

impl From<PathInfo2> for proto::PathInfo {
    fn from(value: PathInfo2) -> Self {
        proto::PathInfo {
            node: Some(castorepb::Node::from_name_and_node(
                value.store_path.to_string().into_bytes().into(),
                value.node,
            )),
            references: value
                .references
                .iter()
                .map(|reference| Bytes::copy_from_slice(reference.digest()))
                .collect(),
            narinfo: Some(proto::NarInfo {
                nar_size: value.nar_size,
                nar_sha256: Bytes::copy_from_slice(&value.nar_sha256),
                signatures: value
                    .signatures
                    .iter()
                    .map(|sig| proto::nar_info::Signature {
                        name: sig.name().to_string(),
                        data: Bytes::copy_from_slice(sig.bytes()),
                    })
                    .collect(),
                reference_names: value.references.iter().map(|r| r.to_string()).collect(),
                deriver: value.deriver.as_ref().map(|sp| proto::StorePath {
                    name: (*sp.name()).to_owned(),
                    digest: Bytes::copy_from_slice(sp.digest()),
                }),
                ca: value.ca.as_ref().map(|ca| ca.into()),
            }),
        }
    }
}

impl TryFrom<crate::proto::PathInfo> for PathInfo2 {
    type Error = ValidatePathInfoError;
    fn try_from(value: crate::proto::PathInfo) -> Result<Self, Self::Error> {
        let narinfo = value
            .narinfo
            .ok_or_else(|| ValidatePathInfoError::NarInfoFieldMissing)?;

        // ensure the references have the right number of bytes.
        for (i, reference) in value.references.iter().enumerate() {
            if reference.len() != store_path::DIGEST_SIZE {
                return Err(ValidatePathInfoError::InvalidReferenceDigestLen(
                    i,
                    reference.len(),
                ));
            }
        }

        // ensure the number of references there matches PathInfo.references count.
        if narinfo.reference_names.len() != value.references.len() {
            return Err(ValidatePathInfoError::InconsistentNumberOfReferences(
                value.references.len(),
                narinfo.reference_names.len(),
            ));
        }

        // parse references in reference_names.
        let mut references = vec![];
        for (i, reference_name_str) in narinfo.reference_names.iter().enumerate() {
            // ensure thy parse as (non-absolute) store path
            let reference_names_store_path =
                StorePathRef::from_bytes(reference_name_str.as_bytes()).map_err(|_| {
                    ValidatePathInfoError::InvalidNarinfoReferenceName(
                        i,
                        reference_name_str.to_owned(),
                    )
                })?;

            // ensure their digest matches the one at self.references[i].
            {
                // This is safe, because we ensured the proper length earlier already.
                let reference_digest = value.references[i].to_vec().try_into().unwrap();

                if reference_names_store_path.digest() != &reference_digest {
                    return Err(
                        ValidatePathInfoError::InconsistentNarinfoReferenceNameDigest(
                            i,
                            reference_digest,
                            *reference_names_store_path.digest(),
                        ),
                    );
                } else {
                    references.push(reference_names_store_path.to_owned());
                }
            }
        }

        let nar_sha256_length = narinfo.nar_sha256.len();

        // split value.node into the name and node components
        let (name, node) = value
            .node
            .ok_or_else(|| ValidatePathInfoError::NoNodePresent)?
            .into_name_and_node()
            .map_err(ValidatePathInfoError::InvalidRootNode)?;

        Ok(Self {
            // value.node has a valid name according to the castore model but might not parse to a
            // [StorePath]
            store_path: StorePath::from_bytes(name.as_ref()).map_err(|err| {
                ValidatePathInfoError::InvalidNodeName(name.as_ref().to_vec(), err)
            })?,
            node,
            references,
            nar_size: narinfo.nar_size,
            nar_sha256: narinfo.nar_sha256.to_vec()[..]
                .try_into()
                .map_err(|_| ValidatePathInfoError::InvalidNarSha256DigestLen(nar_sha256_length))?,
            // If the Deriver field is populated, ensure it parses to a
            // [StorePath].
            // We can't check for it to *not* end with .drv, as the .drv files produced by
            // recursive Nix end with multiple .drv suffixes, and only one is popped when
            // converting to this field.
            deriver: narinfo
                .deriver
                .map(|deriver| {
                    StorePath::from_name_and_digest(&deriver.name, &deriver.digest)
                        .map_err(ValidatePathInfoError::InvalidDeriverField)
                })
                .transpose()?,
            ca: narinfo
                .ca
                .as_ref()
                .map(TryFrom::try_from)
                .transpose()
                .map_err(ValidatePathInfoError::InvalidCaField)?,
            signatures: narinfo
                .signatures
                .into_iter()
                .enumerate()
                .map(|(i, signature)| {
                    signature.data.to_vec()[..]
                        .try_into()
                        .map_err(|_| {
                            ValidatePathInfoError::InvalidSignature(
                                i,
                                SignatureError::InvalidSignatureLen(signature.data.len()),
                            )
                        })
                        .map(|signature_data| Signature::new(signature.name, signature_data))
                })
                .collect::<Result<Vec<_>, ValidatePathInfoError>>()?,
        })
    }
}
