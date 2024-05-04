use async_stream::try_stream;
use data_encoding::BASE64;
use futures::stream::BoxStream;
use prost::Message;
use redb::{Database, ReadableTable, TableDefinition};
use std::path::Path;
use tonic::async_trait;
use tracing::{instrument, warn};
use tvix_castore::Error;

use crate::proto::PathInfo;

use super::PathInfoService;

const PATHINFO_TABLE: TableDefinition<[u8; 20], Vec<u8>> = TableDefinition::new("pathinfo");

/// PathInfoService implementation using redb under the hood.
pub struct RedbPathInfoService {
    db: Database,
}

impl RedbPathInfoService {
    /// Constructs a new instance using the in-memory backend.
    pub fn new_temporary() -> Result<Self, redb::Error> {
        let db =
            redb::Database::builder().create_with_backend(redb::backends::InMemoryBackend::new())?;

        create_schema(&db)?;

        Ok(Self { db })
    }

    /// Constructs a new instance using the specified file system path for
    /// storage.
    pub async fn open<P: AsRef<Path>>(p: P) -> Result<Self, redb::Error> {
        // TODO: spawn_blocking
        let db = redb::Database::builder().create(p)?;

        create_schema(&db)?;

        Ok(Self { db })
    }
}

/// Ensures all tables are present.
fn create_schema(db: &redb::Database) -> Result<(), redb::Error> {
    // opens a write transaction and calls open_table on PATHINFO_TABLE, which will
    // create it if not present.
    let txn = db.begin_write()?;
    txn.open_table(PATHINFO_TABLE)?;
    txn.commit()?;

    Ok(())
}

// TODO: error handling, spawn_blocking

#[async_trait]
impl PathInfoService for RedbPathInfoService {
    #[instrument(level = "trace", skip_all, fields(path_info.digest = BASE64.encode(&digest)))]
    async fn get(&self, digest: [u8; 20]) -> Result<Option<PathInfo>, Error> {
        let txn = self.db.begin_read().expect("read txn");
        let table = txn.open_table(PATHINFO_TABLE).expect("open table");
        match table.get(digest).expect("get error") {
            Some(pathinfo_bytes) => Ok(Some(
                PathInfo::decode(pathinfo_bytes.value().as_slice()).map_err(|e| {
                    warn!("failed to decode stored PathInfo: {}", e);
                    Error::StorageError(format!("failed to decode stored PathInfo: {}", e))
                })?,
            )),
            None => Ok(None),
        }
    }

    #[instrument(level = "trace", skip_all, fields(path_info.root_node = ?path_info.node))]
    async fn put(&self, path_info: PathInfo) -> Result<PathInfo, Error> {
        // Call validate on the received PathInfo message.
        let store_path = path_info
            .validate()
            .map_err(|e| Error::InvalidRequest(format!("failed to validate PathInfo: {}", e)))?;

        let txn = self.db.begin_write().expect("write txn");
        {
            let mut table = txn.open_table(PATHINFO_TABLE).expect("write txn");

            table
                .insert(store_path.digest(), path_info.encode_to_vec())
                .map_err(|e| {
                    warn!("failed to insert PathInfo: {}", e);
                    Error::StorageError(format!("failed to insert PathInfo: {}", e))
                })?;
        }

        txn.commit().map_err(|e| {
            warn!("failed to commit: {}", e);
            Error::StorageError(format!("failed to commit: {}", e))
        })?;

        Ok(path_info)
    }

    fn list(&self) -> BoxStream<'static, Result<PathInfo, Error>> {
        let read_txn = self.db.begin_read().expect("read txn");
        let table = read_txn.open_table(PATHINFO_TABLE).expect("open table");

        Box::pin(try_stream! {
            for elem in table.iter().expect("iter") {
                let elem = elem.expect("fail access elem");
                yield PathInfo::decode(elem.1.value().as_slice()).map_err(|e| Error::InvalidRequest(format!("invalid PathInfo: {}", e)))?;
            }
        })
    }
}
