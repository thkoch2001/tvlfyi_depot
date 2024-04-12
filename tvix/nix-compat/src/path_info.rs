use crate::{narinfo, store_path::StorePathRef};
use serde::{Deserialize, Serialize};
use std::collections::BTreeSet;

/// Represents information about a Store Path.
/// Used in Nix' `exportReferencesGraph` feature, or `nix path-info --json`.
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct PathInfo<'a> {
    #[serde(rename = "closureSize", skip_serializing_if = "Option::is_none")]
    pub closure_size: Option<u64>,
    #[serde(borrow)]
    pub deriver: Option<StorePathRef<'a>>,

    #[serde(
        rename = "narHash",
        serialize_with = "as_sri_str",
        deserialize_with = "from_sha256_sri"
    )]
    pub nar_sha256: [u8; 32],

    #[serde(rename = "narSize")]
    pub(crate) nar_size: u64,
    #[serde(borrow)]
    pub path: StorePathRef<'a>,

    /// The list of other Store Paths this Store Path refers to.
    /// StorePathRef does Ord by the nixbase32-encoded string repr, so this is correct.
    pub references: BTreeSet<StorePathRef<'a>>,

    #[serde(rename = "registrationTime", skip_serializing_if = "Option::is_none")]
    pub registration_time: Option<u64>,

    /// Ed25519 signature over the path fingerprint
    pub signatures: Vec<narinfo::Signature<'a>>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub ultimate: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub valid: Option<bool>,
}

fn as_sri_str<S>(v: &[u8; 32], serializer: S) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    let string = format!("sha256-{}", data_encoding::BASE64.encode(v));
    string.serialize(serializer)
}

fn from_sha256_sri<'de, D>(deserializer: D) -> Result<[u8; 32], D::Error>
where
    D: serde::Deserializer<'de>,
{
    let str: &'de str = Deserialize::deserialize(deserializer)?;

    let digest_str = str.strip_prefix("sha256-").ok_or_else(|| {
        serde::de::Error::invalid_value(serde::de::Unexpected::Str(str), &"sha256-…")
    })?;

    // needs one additional byte?
    let mut digest: [u8; 32 + 1] = [0; 32 + 1];

    let decoded_len = data_encoding::BASE64
        .decode_mut(digest_str.as_bytes(), &mut digest)
        .map_err(|_| {
            serde::de::Error::invalid_value(serde::de::Unexpected::Str(str), &"valid base64")
        })?;

    if decoded_len != 32 {
        return Err(serde::de::Error::invalid_value(
            serde::de::Unexpected::Str(str),
            &"32 bytes of decoded digest",
        ));
    }

    Ok(digest[0..32].try_into().unwrap())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn serialize_deserialize() {
        // `nix path-info $(nix-build -A hello) --json --argstr system x86_64-linux` at
        // @nixpkgs f1e4c7a12aba0223e1f19757e1483d848f578ebe
        let pathinfo_actual = PathInfo {
            closure_size: None,
            deriver: Some(
                StorePathRef::from_bytes(b"4h712p20bpisada9lir5nyd073ybs7nw-hello-2.12.1.drv")
                    .unwrap(),
            ),
            nar_sha256: data_encoding::BASE64
                .decode(b"vfDN6LylLxKXQD7NofvAt4tsCGwL/DNvyFWz1iVcnyo=")
                .unwrap()
                .try_into()
                .unwrap(),
            nar_size: 226560,
            path: StorePathRef::from_bytes(b"dbghhbq1x39yxgkv3vkgfwbxrmw9nfzi-hello-2.12.1")
                .unwrap(),
            references: [
                b"dbghhbq1x39yxgkv3vkgfwbxrmw9nfzi-hello-2.12.1",
                b"ddwyrxif62r8n6xclvskjyy6szdhvj60-glibc-2.39-5",
            ]
            .iter()
            .map(|b| StorePathRef::from_bytes(&b[..]).unwrap())
            .collect(),
            signatures: ["cache.nixos.org-1:ug43SEwiXcwCT9abPhZp39VG9RMGEgtpwhUhY6Zr6GPSUe0oB3J2KUXYB3ikxvppwJe0joUA9gvcOoZHvksjCQ=="].iter().map(|s| {
                narinfo::Signature::parse(s).unwrap()
            }).collect(),
            registration_time: Some(1712919696),
            ultimate: None,
            valid: Some(true),
        };

        let pathinfo_str_json = r#"{"deriver":"/nix/store/4h712p20bpisada9lir5nyd073ybs7nw-hello-2.12.1.drv","narHash":"sha256-vfDN6LylLxKXQD7NofvAt4tsCGwL/DNvyFWz1iVcnyo=","narSize":226560,"path":"/nix/store/dbghhbq1x39yxgkv3vkgfwbxrmw9nfzi-hello-2.12.1","references":["/nix/store/dbghhbq1x39yxgkv3vkgfwbxrmw9nfzi-hello-2.12.1","/nix/store/ddwyrxif62r8n6xclvskjyy6szdhvj60-glibc-2.39-5"],"registrationTime":1712919696,"signatures":["cache.nixos.org-1:ug43SEwiXcwCT9abPhZp39VG9RMGEgtpwhUhY6Zr6GPSUe0oB3J2KUXYB3ikxvppwJe0joUA9gvcOoZHvksjCQ=="],"valid":true}"#;

        let serialized = serde_json::to_string(&pathinfo_actual).expect("must serialize");
        assert_eq!(pathinfo_str_json, &serialized);

        let deserialized: PathInfo<'_> =
            serde_json::from_str(pathinfo_str_json).expect("must deserialize");
        assert_eq!(&pathinfo_actual, &deserialized);
    }
}
