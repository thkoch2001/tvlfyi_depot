use crate::proto::PathInfo;
use async_stream::try_stream;
use data_encoding::BASE64;
use futures::stream::BoxStream;
use prost::Message;
use redb::{Database, ReadableTable, TableDefinition};
use std::path::Path;
use tonic::async_trait;
use tracing::instrument;

use super::Error;
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
        let txn = self
            .db
            .begin_read()
            .map_err(|e| Error::Retrieve(Box::new(e), "failed to open read transaction"))?;

        let table = txn
            .open_table(PATHINFO_TABLE)
            .map_err(|e| Error::Retrieve(Box::new(e), "failed to open table"))?;

        if let Some(pathinfo_bytes) = table
            .get(digest)
            .map_err(|e| Error::Retrieve(Box::new(e), "failed to get from table"))?
        {
            Ok(Some(
                PathInfo::decode(pathinfo_bytes.value().as_slice()).map_err(|e| {
                    Error::Retrieve(Box::new(e), "failed to decode stored pathinfo")
                })?,
            ))
        } else {
            Ok(None)
        }
    }

    #[instrument(level = "trace", skip_all, fields(path_info.root_node = ?path_info.node))]
    async fn put(&self, path_info: PathInfo) -> Result<PathInfo, Error> {
        // Call validate on the received PathInfo message.
        let store_path = path_info.validate()?;

        let txn = self
            .db
            .begin_write()
            .map_err(|e| Error::Insert(Box::new(e), "failed to open write transaction"))?;
        {
            let mut table = txn
                .open_table(PATHINFO_TABLE)
                .map_err(|e| Error::Insert(Box::new(e), "failed to open table"))?;

            table
                .insert(store_path.digest(), path_info.encode_to_vec())
                .map_err(|e| Error::Insert(Box::new(e), "failed to insert into table"))?;
        }

        txn.commit()
            .map_err(|e| Error::Insert(Box::new(e), "failed to commit transaction"))?;

        Ok(path_info)
    }

    fn list(&self) -> BoxStream<'static, Result<PathInfo, Error>> {
        Box::pin(try_stream! {
            let txn = self
                .db
                .begin_read()
                .map_err(|e| Error::Retrieve(Box::new(e), "failed to open read transaction"))?;

            let table = txn.open_table(PATHINFO_TABLE)
                .map_err(|e| Error::Retrieve(Box::new(e), "failed to open table"))?;


            for elem in table.iter().map_err(|e| Error::Retrieve(Box::new(e), "failed to iterate over table items" ))? {
                let (_key, value) = elem.map_err(|e| Error::Retrieve(Box::new(e), "failed to access item"))?;
                let pathinfo_bytes = value.value().as_slice();

                yield PathInfo::decode(pathinfo_bytes).map_err(|e| Error::Retrieve(Box::new(e), "failed to decode stored pathinfo"))?;
            }
        })
    }
}
