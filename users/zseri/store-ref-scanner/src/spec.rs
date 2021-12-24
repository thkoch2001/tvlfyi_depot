use crate::hbm::HalfBytesMask;
use camino::Utf8PathBuf;
use once_cell::sync::Lazy;

pub struct StoreSpec {
    /// path to store without trailing slash
    pub path_to_store: Utf8PathBuf,

    /// compressed map of allowed ASCII characters in hash part
    pub valid_hashbytes: HalfBytesMask,

    /// compressed map of allowed ASCII characters in part after hash
    pub valid_restbytes: HalfBytesMask,

    /// exact length of hash part of store paths
    pub hashbytes_len: u8,
}

impl StoreSpec {
    pub(crate) fn check_rest(&self, rest: &[u8]) -> bool {
        let hbl = self.hashbytes_len.into();
        rest.iter()
            .take(hbl)
            .take_while(|&&i| self.valid_hashbytes.contains(i))
            .count()
            == hbl
    }
}

pub static SPEC_DFL_NIX2: Lazy<StoreSpec> = Lazy::new(|| StoreSpec {
    path_to_store: "/nix/store".into(),
    valid_hashbytes: HalfBytesMask::B32_REVSHA256,
    valid_restbytes: HalfBytesMask::from_bytes(
        b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+-._?=",
    ),
    hashbytes_len: 32,
});

pub static SPEC_DFL_YZIX1: Lazy<StoreSpec> = Lazy::new(|| StoreSpec {
    path_to_store: "/yzixs".into(),
    valid_hashbytes: HalfBytesMask::B64_BLAKE2B256,
    valid_restbytes: HalfBytesMask::from_bytes(
        b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+-._?=",
    ),
    hashbytes_len: 43,
});
