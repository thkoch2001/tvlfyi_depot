use super::{Error, StorePath, StorePathRef};

use std::borrow::Borrow;

/// Allows generic functions generic over types that are easily
/// convertible to `StorePathRef`.
pub trait AsStorePathRef {
    fn as_store_path_ref(&self) -> Result<impl Borrow<StorePathRef<'_>>, Error>;
}

impl AsStorePathRef for StorePathRef<'_> {
    fn as_store_path_ref(&self) -> Result<impl Borrow<StorePathRef<'_>>, Error> {
        Ok(self)
    }
}

impl AsStorePathRef for StorePath {
    fn as_store_path_ref(&self) -> Result<impl Borrow<StorePathRef<'_>>, Error> {
        Ok::<StorePathRef<'_>, _>(self.into())
    }
}

// This implementation greatly eases the transition from `String`-ly
// typed store paths to `StorePath`s. Afterwards, it should probably
// be removed.
impl AsStorePathRef for str {
    fn as_store_path_ref(&self) -> Result<impl Borrow<StorePathRef<'_>>, Error> {
        StorePathRef::from_absolute_path(self.as_bytes())
    }
}

// See implementation for str
impl AsStorePathRef for String {
    fn as_store_path_ref(&self) -> Result<impl Borrow<StorePathRef<'_>>, Error> {
        let str = self as &str;
        str.as_store_path_ref()
    }
}

#[cfg(test)]
mod test {
    use crate::store_path::StorePathRef;

    use super::AsStorePathRef;
    use std::borrow::Borrow;

    const VALID_STORE_PATH: &str =
        "/nix/store/00bgd045z0d4icpbc2yyz4gx48ak44la-net-tools-1.60_p20170221182432";

    #[test]
    fn str_round_trip() -> Result<(), super::Error> {
        let store_path_ref = VALID_STORE_PATH.as_store_path_ref()?;
        let store_path_ref: &StorePathRef<'_> = store_path_ref.borrow();

        assert_eq!(VALID_STORE_PATH, store_path_ref.to_absolute_path());
        Ok(())
    }

    #[test]
    fn string_round_trip() -> Result<(), super::Error> {
        let store_path = String::from(VALID_STORE_PATH);
        let store_path_ref = store_path.as_store_path_ref()?;
        let store_path_ref: &StorePathRef<'_> = store_path_ref.borrow();

        assert_eq!(VALID_STORE_PATH, store_path_ref.to_absolute_path());
        Ok(())
    }
}
