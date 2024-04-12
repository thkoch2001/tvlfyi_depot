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

/// PathInfo are ordered by their `path` field.
impl Ord for PathInfo<'_> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.path.cmp(&other.path)
    }
}

impl PartialOrd for PathInfo<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(&other))
    }
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

    /// Ensure we can create the same JSON as the exportReferencesGraph feature
    #[test]
    fn serialize_deserialize_export_references_graph() {
        // JSON extracted from a build of
        // stdenv.mkDerivation { name = "hello"; __structuredAttrs = true; exportReferencesGraph.blub = [ pkgs.hello ]; nativeBuildInputs = [pkgs.jq]; buildCommand = "jq -rc .blub $NIX_ATTRS_JSON_FILE > $out"; }
        let pathinfos_str_json = r#"[{"closureSize":1828984,"narHash":"sha256:11vm2x1ajhzsrzw7lsyss51mmr3b6yll9wdjn51bh7liwkpc8ila","narSize":1828984,"path":"/nix/store/7n0mbqydcipkpbxm24fab066lxk68aqk-libunistring-1.1","references":["/nix/store/7n0mbqydcipkpbxm24fab066lxk68aqk-libunistring-1.1"],"valid":true},{"closureSize":32696176,"narHash":"sha256:0alzbhjxdcsmr1pk7z0bdh46r2xpq3xs3k9y82bi4bx5pklcvw5x","narSize":226560,"path":"/nix/store/dbghhbq1x39yxgkv3vkgfwbxrmw9nfzi-hello-2.12.1","references":["/nix/store/dbghhbq1x39yxgkv3vkgfwbxrmw9nfzi-hello-2.12.1","/nix/store/ddwyrxif62r8n6xclvskjyy6szdhvj60-glibc-2.39-5"],"valid":true},{"closureSize":32469616,"narHash":"sha256:1zw5p05fh0k836ybfxkskv8apcv2m3pm2wa6y90wqn5w5kjyj13c","narSize":30119936,"path":"/nix/store/ddwyrxif62r8n6xclvskjyy6szdhvj60-glibc-2.39-5","references":["/nix/store/ddwyrxif62r8n6xclvskjyy6szdhvj60-glibc-2.39-5","/nix/store/rxganm4ibf31qngal3j3psp20mak37yy-xgcc-13.2.0-libgcc","/nix/store/s32cldbh9pfzd9z82izi12mdlrw0yf8q-libidn2-2.3.7"],"valid":true},{"closureSize":159560,"narHash":"sha256:10q8iyvfmpfck3yiisnj1j8vp6lq3km17r26sr95zpdf9mgmk69s","narSize":159560,"path":"/nix/store/rxganm4ibf31qngal3j3psp20mak37yy-xgcc-13.2.0-libgcc","references":[],"valid":true},{"closureSize":2190120,"narHash":"sha256:1cv997nzxbd91jhmzwnhxa1ahlzp5ffli8m4a5npcq8zg0vb1kwg","narSize":361136,"path":"/nix/store/s32cldbh9pfzd9z82izi12mdlrw0yf8q-libidn2-2.3.7","references":["/nix/store/7n0mbqydcipkpbxm24fab066lxk68aqk-libunistring-1.1","/nix/store/s32cldbh9pfzd9z82izi12mdlrw0yf8q-libidn2-2.3.7"],"valid":true}]"#;

        // We ensure it roundtrips (to check the sorting is correct)
        let deserialized: BTreeSet<PathInfo> =
            serde_json::from_str(pathinfos_str_json).expect("must serialize");

        let serialized_again = serde_json::to_string(&deserialized).expect("must deserialize");
        assert_eq!(pathinfos_str_json, serialized_again);

        // Also compare one specific item to be populated as expected.
        assert_eq!(
            &PathInfo {
                closure_size: Some(1828984),
                deriver: None,
                nar_sha256: data_encoding::BASE64
                    .decode(b"11vm2x1ajhzsrzw7lsyss51mmr3b6yll9wdjn51bh7liwkpc8ila")
                    .expect("must decode")
                    .try_into()
                    .unwrap(),
                nar_size: 1828984,
                path: StorePathRef::from_bytes(
                    b"7n0mbqydcipkpbxm24fab066lxk68aqk-libunistring-1.1"
                )
                .expect("must parse"),
                references: BTreeSet::from_iter([StorePathRef::from_bytes(
                    b"7n0mbqydcipkpbxm24fab066lxk68aqk-libunistring-1.1"
                )
                .unwrap()]),
                registration_time: None,
                signatures: vec![],
                ultimate: None,
                valid: Some(true),
            },
            deserialized.first().unwrap()
        );
    }
}
