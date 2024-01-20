//! Implements `builtins.path`, a low-level primitive to insert paths in the store.

use crate::builtins::ImportError;
use futures::pin_mut;
use std::path::Path;
use tvix_eval::{
    builtin_macros::builtins,
    generators::{self, GenCo},
    ErrorKind, Value,
};

use std::rc::Rc;

async fn filtered_ingest(
    state: Rc<TvixStoreIO>,
    co: GenCo,
    path: &Path,
    filter: Option<&Value>,
) -> Result<tvix_castore::proto::node::Node, ErrorKind> {
    let mut entries_per_depths: Vec<Vec<walkdir::DirEntry>> = vec![Vec::new()];
    let mut it = walkdir::WalkDir::new(path)
        .follow_links(false)
        .follow_root_links(false)
        .contents_first(false)
        .sort_by_file_name()
        .into_iter();

    // Skip root node.
    entries_per_depths[0].push(
        it.next()
            .expect("Failed to obtain root node")
            .expect("Failed to read root node"),
    );

    while let Some(entry) = it.next() {
        // Entry could be a NotFound, if the root path specified does not exist.
        let entry = entry.expect("Failed to find the entry");

        let file_type = if entry.file_type().is_dir() {
            "directory"
        } else if entry.file_type().is_file() {
            "file"
        } else if entry.file_type().is_symlink() {
            "symlink"
        } else {
            "other"
        };

        let should_keep: bool = if let Some(filter) = filter {
            generators::request_force(
                &co,
                generators::request_call_with(
                    &co,
                    filter.clone(),
                    [
                        Value::String(entry.path().to_string_lossy().as_ref().into()),
                        Value::String(file_type.into()),
                    ],
                )
                .await,
            )
            .await
            .as_bool()?
        } else {
            true
        };

        if !should_keep {
            println!("Skipping {:?}...", entry);
            if file_type == "directory" {
                it.skip_current_dir();
            }
            continue;
        } else {
            println!("Keeping {:?}...", entry);
        }

        if entry.depth() >= entries_per_depths.len() {
            debug_assert!(
                entry.depth() == entries_per_depths.len(),
                "We should not be allocating more than one level at once, requested node at depth {}, had entries per depth containing {} levels",
                entry.depth(),
                entries_per_depths.len()
            );

            entries_per_depths.push(vec![entry]);
        } else {
            entries_per_depths[entry.depth()].push(entry);
        }

        // FUTUREWORK: determine when it's the right moment to flush a level to the ingester.
    }

    let entries_stream = tvix_castore::import::leveled_entries_to_stream(entries_per_depths);

    pin_mut!(entries_stream);

    Ok(state
        .ingest_entries_sync(entries_stream)
        .expect("Failed to ingest entries"))
}

// 4KiB those days is the sector size, some disks may be dumb and use 512 bytes (the old constant).
// FUTUREWORK: blob storage can be highly optimized for heavy read usecases, with that in mind,
// some advanced users may want to configure this constant to 128KiB for example.
const REASONABLE_BLOCK_SIZE: usize = 4 * 1024;

#[builtins(state = "Rc<TvixStoreIO>")]
mod import_builtins {
    use std::rc::Rc;

    use super::*;

    use nix_compat::nixhash;
    use tvix_eval::generators::Gen;
    use tvix_eval::{generators::GenCo, ErrorKind, Value};

    use sha2::{Digest, Sha256};
    use tokio::io::AsyncReadExt;
    use tvix_castore::B3Digest;

    use crate::tvix_store_io::TvixStoreIO;

    #[builtin("path")]
    async fn builtin_path(
        state: Rc<TvixStoreIO>,
        co: GenCo,
        args: Value,
    ) -> Result<Value, ErrorKind> {
        let args = args.to_attrs()?;
        let path = args.select_required("path")?;
        let path = generators::request_force(&co, path.clone())
            .await
            .to_path()?;
        let name: String = if let Some(name) = args.select("name") {
            generators::request_force(&co, name.clone())
                .await
                .to_str()?
                .as_str()
                .to_string()
        } else {
            tvix_store::import::path_to_name(&path)
                .expect("Failed to derive the default name out of the path")
                .to_string()
        };
        let filter = args.select("filter");
        let recursive_ingestion = args
            .select("recursive")
            .map(|r| r.as_bool())
            .transpose()?
            .unwrap_or(true); // Yes, yes, Nix, by default, puts `recursive = true;`.
        let expected_sha256 = args
            .select("sha256")
            .map(|h| {
                h.to_str().and_then(|expected| {
                    nix_compat::nixhash::from_str(expected.as_ref(), None).map_err(|_err| {
                        // TODO: a better error would be nice, we use
                        // DerivationError::InvalidOutputHash usually for derivation construction.
                        // This is not a derivation construction, should we move it outside and
                        // generalize?
                        ErrorKind::TypeError {
                            expected: "sha256",
                            actual: "not a sha256",
                        }
                    })
                })
            })
            .transpose()?;

        if !recursive_ingestion && !std::fs::metadata(path.as_ref())?.is_file() {
            return Err(
                ImportError::FlatImportOfNonFile(path.to_string_lossy().to_string()).into(),
            );
        }

        let root_node = filtered_ingest(state.clone(), co, path.as_ref(), filter).await?;
        let file_digest: Option<B3Digest> = match root_node {
            tvix_castore::proto::node::Node::File(ref fnode) => {
                Some(fnode.digest.clone().try_into().expect(
                    "Tvix bug: expected the digest to fit in the B3Digest length (32 bytes)",
                ))
            }
            _ => None,
        };

        let (path_info, output_path) =
            state.node_to_path_info_sync(name.as_ref(), path.as_ref(), root_node)?;

        if let Some(expected_sha256) = expected_sha256 {
            let nar_hash: [u8; 32] = if recursive_ingestion {
                path_info
                    .narinfo
                    .as_ref()
                    .expect("Tvix bug: expected a recursive FOD PathInfo")
                    .nar_sha256
                    .as_ref()
                    .try_into()
                    .expect("Tvix bug: expected a SHA-256 digest")
            } else {
                // We cannot hash anything else than file in flat import mode.
                let digest: B3Digest = match file_digest {
                    Some(digest) => digest,
                    None => {
                        return Err(ImportError::FlatImportOfNonFile(
                            path.to_string_lossy().to_string(),
                        )
                        .into());
                    }
                };

                state
                    .tokio_handle
                    .block_on(async {
                        let reader = state.blob_service.open_read(&digest).await?;
                        if let Some(mut reader) = reader {
                            let mut hasher = Sha256::new();
                            // We never need to have the whole file in memory,
                            // we can hash it in constant space.
                            let mut buffer: [u8; REASONABLE_BLOCK_SIZE] = [0; REASONABLE_BLOCK_SIZE];

                            while reader.read(&mut buffer).await? > 0 {
                                hasher.update(buffer);
                            }
                            Ok::<_, std::io::Error>(hasher.finalize())
                        } else {
                            panic!("Tvix bug: expected to read blob '{}' we just ingested, but didn't find it in the blob store", digest);
                        }
                    })?
                    .into()
            };

            if nar_hash != expected_sha256.digest_as_bytes() {
                return Err(ImportError::HashMismatch(
                    path.to_string_lossy().to_string(),
                    expected_sha256.to_nix_hex_string(),
                    nixhash::from_algo_and_digest(nixhash::HashAlgo::Sha256, &nar_hash)
                        .expect("Tvix bug: failed to convert the SHA-256 into a Nix SHA-256 representation")
                        .to_nix_hex_string(),
                ).into());
            }
        }

        let _ = state.put_to_path_info_sync(path_info);

        Ok(output_path.to_absolute_path().into())
    }

    #[builtin("filterSource")]
    async fn builtin_filter_source(
        state: Rc<TvixStoreIO>,
        co: GenCo,
        #[lazy] filter: Value,
        path: Value,
    ) -> Result<Value, ErrorKind> {
        let p = path.to_path()?;
        let root_node = filtered_ingest(state.clone(), co, &p, Some(&filter)).await?;
        let name = tvix_store::import::path_to_name(&p)?;

        Ok(state
            .register_node_in_path_info_service_sync(name, &p, root_node)
            .expect("Failed to register the node in path info")
            .to_absolute_path()
            .into())
    }
}

pub use import_builtins::builtins as import_builtins;

use crate::tvix_store_io::TvixStoreIO;
