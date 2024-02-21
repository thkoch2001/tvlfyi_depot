use crate::nar::calculate_size_and_sha256;

use super::HashTypeRequest;
use nix_compat::nixhash::{HashAlgo, NixHash};
use sha2::{Digest, Sha256};
use tokio_util::io::SyncIoBridge;
use tvix_castore::blobservice::BlobService;
use tvix_castore::directoryservice::DirectoryService;
use tvix_castore::Error;

/// Helper function, used to provide calculate_digest() functionality in the PathInfoService trait.
pub(crate) async fn calculate_digest<BS, DS>(
    blob_service: BS,
    directory_service: DS,
    hash_type_request: &HashTypeRequest<'_>,
) -> Result<(NixHash, u64), Error>
where
    BS: AsRef<dyn BlobService> + Send + Sync,
    DS: AsRef<dyn DirectoryService> + Send + Sync,
{
    match hash_type_request {
        HashTypeRequest::Flat(algo, blob_digest) => {
            match blob_service.as_ref().open_read(blob_digest).await? {
                Some(blob_reader) => {
                    // spawn a blocking task hashing through the data.
                    let mut blob_reader = SyncIoBridge::new(blob_reader);
                    let blob_digest = tokio::task::spawn_blocking(move || {
                        // TODO: handle all algos, return NixHash
                        let mut hasher = Sha256::new();
                        std::io::copy(&mut blob_reader, &mut hasher)?;
                        Ok::<[u8; 32], std::io::Error>(hasher.finalize().into())
                    })
                    .await??;

                    Ok((NixHash::Sha256(blob_digest), 0))
                }
                None => return Err(Error::StorageError("blob not found".to_string())),
            }
        }
        HashTypeRequest::Nar(algo, root_node) => {
            // TODO: do we need to do this?
            if *algo != HashAlgo::Sha256 {
                return Err(Error::InvalidRequest(
                    "calculate_digest with nar and non-sha256 is not supported".to_string(),
                ));
            }
            calculate_size_and_sha256(root_node, blob_service, directory_service)
                .await
                .map_err(|e| Error::StorageError(e.to_string()))
                .map(|(nar_size, nar_sha256)| (NixHash::Sha256(nar_sha256), nar_size))
        }
    }
}
