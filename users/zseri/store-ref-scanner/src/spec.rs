use crate::hbm::HalfBytesMask;

pub struct StoreSpec<'path> {
    /// path to store without trailing slash
    pub path_to_store: &'path str,

    /// compressed map of allowed ASCII characters in hash part
    pub valid_hashbytes: HalfBytesMask,

    /// compressed map of allowed ASCII characters in part after hash
    pub valid_restbytes: HalfBytesMask,

    /// exact length of hash part of store paths
    pub hashbytes_len: u8,
}

impl StoreSpec<'_> {
    pub(crate) fn check_rest(&self, rest: &[u8]) -> bool {
        let hbl = self.hashbytes_len.into();
        rest.iter()
            .take(hbl)
            .take_while(|&&i| self.valid_hashbytes.contains(i))
            .count()
            == hbl
    }

    pub const DFL_NIX2: StoreSpec<'static> = StoreSpec {
        path_to_store: "/nix/store",
        valid_hashbytes: HalfBytesMask::B32_REVSHA256,
        valid_restbytes: HalfBytesMask::DFL_REST,
        hashbytes_len: 32,
    };

    pub const DFL_YZIX1: StoreSpec<'static> = StoreSpec {
        path_to_store: "/yzixs",
        valid_hashbytes: HalfBytesMask::B64_BLAKE2B256,
        valid_restbytes: HalfBytesMask::DFL_REST,
        hashbytes_len: 43,
    };
}
