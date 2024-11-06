use std::future::Future;

use super::{
    de::{NixDeserialize, NixRead},
    ser::{NixSerialize, NixWrite},
};

/// Optional type in Nix wire protocol.
///
/// Uses a bool to indicate presence.
/// Encoding/decoding always starts with a bool and then followed by payload if bool is true.
///
/// Example uses: QuerySubstitutablePathInfo, QueryPathInfo. See serialization.md for details.
#[derive(Debug)]
pub struct OptionWithPresence<T> {
    pub value: Option<T>,
}

impl<T> From<Option<T>> for OptionWithPresence<T> {
    fn from(value: Option<T>) -> Self {
        Self { value }
    }
}

impl<T> From<T> for OptionWithPresence<T> {
    fn from(value: T) -> Self {
        Self { value: Some(value) }
    }
}

impl<T> NixSerialize for OptionWithPresence<T>
where
    T: NixSerialize + Sync + Send,
{
    async fn serialize<W>(&self, writer: &mut W) -> Result<(), W::Error>
    where
        W: NixWrite + Send,
    {
        match &self.value {
            Some(inner) => {
                writer.write_value(&true).await?;
                writer.write_value(inner).await
            }
            None => writer.write_value(&false).await,
        }
    }
}

impl<T> NixDeserialize for OptionWithPresence<T>
where
    T: NixDeserialize + Send,
{
    fn try_deserialize<R>(
        reader: &mut R,
    ) -> impl Future<Output = Result<Option<Self>, R::Error>> + Send + '_
    where
        R: ?Sized + NixRead + Send,
    {
        async move {
            if let Some(presence) = reader.try_read_value().await? {
                if presence {
                    if let Some(result) = reader.try_read_value::<T>().await? {
                        return Ok(Some(Self {
                            value: Some(result),
                        }));
                    }
                }
            }
            Ok(None)
        }
    }
}

/// Optional type in Nix wire protocol.
///
/// Uses an "empty/default" value to indicate absence.
///
/// Example uses: OptContentAddress, OptStorePath. See serialization.md for details.
#[derive(Debug)]
pub struct OptionOrDefault<T> {
    pub value: Option<T>,
}

impl<T> From<Option<T>> for OptionOrDefault<T> {
    fn from(value: Option<T>) -> Self {
        Self { value }
    }
}

impl<T> From<T> for OptionOrDefault<T> {
    fn from(value: T) -> Self {
        Self { value: Some(value) }
    }
}

impl<T> NixSerialize for OptionOrDefault<T>
where
    T: NixSerialize + Default + Sync + Send,
{
    async fn serialize<W>(&self, writer: &mut W) -> Result<(), W::Error>
    where
        W: NixWrite + Send,
    {
        match &self.value {
            Some(inner) => writer.write_value(inner).await,
            None => writer.write_value(&T::default()).await,
        }
    }
}
