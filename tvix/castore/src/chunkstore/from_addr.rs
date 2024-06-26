use url::Url;

use crate::Error;

use super::{ChunkStore, MemoryChunkStore, ObjectStoreChunkStore};

/// Constructs a new instance of a [ChunkStore] from an URI.
///
/// The following schemes are supported by the following services:
/// - `memory://` ([MemoryChunkStore])
/// - `objectstore+*://` ([ObjectStoreChunkStore])
///
/// See their `from_url` methods for more details about their syntax.
pub async fn from_addr(uri: &str) -> Result<Box<dyn ChunkStore>, crate::Error> {
    let url = Url::parse(uri)
        .map_err(|e| crate::Error::StorageError(format!("unable to parse url: {}", e)))?;

    let blob_service: Box<dyn ChunkStore> = match url.scheme() {
        "memory" => {
            // memory doesn't support host or path in the URL.
            if url.has_host() || !url.path().is_empty() {
                return Err(Error::StorageError("invalid url".to_string()));
            }
            Box::<MemoryChunkStore>::default()
        }
        scheme if scheme.starts_with("objectstore+") => {
            // We need to convert the URL to string, strip the prefix there, and then
            // parse it back as url, as Url::set_scheme() rejects some of the transitions we want to do.
            let trimmed_url = {
                let s = url.to_string();
                Url::parse(s.strip_prefix("objectstore+").unwrap()).unwrap()
            };
            Box::new(
                ObjectStoreChunkStore::parse_url(&trimmed_url)
                    .map_err(|e| Error::StorageError(e.to_string()))?,
            )
        }
        scheme => {
            return Err(crate::Error::StorageError(format!(
                "unknown scheme: {}",
                scheme
            )))
        }
    };

    Ok(blob_service)
}
