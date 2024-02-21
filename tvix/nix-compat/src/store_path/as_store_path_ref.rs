use super::{StorePath, StorePathRef};

use std::borrow::Borrow;

/// Generic over `StorePath`/`StorePathRef``.
pub trait AsStorePathRef {
    fn as_store_path_ref(&self) -> impl Borrow<StorePathRef<'_>>;
}

impl AsStorePathRef for StorePathRef<'_> {
    fn as_store_path_ref(&self) -> impl Borrow<StorePathRef<'_>> {
        self
    }
}

impl AsStorePathRef for StorePath {
    fn as_store_path_ref(&self) -> impl Borrow<StorePathRef<'_>> {
        <&StorePath as std::convert::Into<StorePathRef<'_>>>::into(self)
    }
}
