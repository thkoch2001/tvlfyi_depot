use bigtable_rs::{bigtable, google::bigtable::v2 as bigtable_v2};
use bytes::Bytes;
use data_encoding::HEXLOWER;
use futures::stream::BoxStream;
use prost::Message;
use tonic::async_trait;
use tracing::{instrument, trace, warn};

use super::{utils::traverse_directory, DirectoryPutter, DirectoryService, SimplePutter};
use crate::{proto, B3Digest, Error};

/// There should not be more than 10 MiB in a single cell.
/// https://cloud.google.com/bigtable/docs/schema-design#cells
const CELL_SIZE_LIMIT: u64 = 10 * 1024 * 1024;

/// Provides a [DirectoryService] implementation using [Bigtable](https://
/// cloud.google.com/bigtable/docs/) as an underlying K/V store.
///
/// # Data format
/// We use Bigtable as a plain K/V store.
/// The row key is the digest of the directory, in hexlower.
/// Inside the row, we currently have a single column/cell, again using the
/// hexlower directory digest.
/// Its value is the Directory message, serialized in canonical protobuf.
/// We currently only populate this column.
///
/// In the future, we might want to introduce "bucketing", essentially storing
/// all directories inserted via `put_multiple_start` in a batched form.
/// This will prevent looking up intermediate Directories, which are not
/// directly at the root, so rely on store composition.
#[derive(Clone)]
pub struct BigtableDirectoryService {
    client: bigtable::BigTable,
    table_name: String,
    family_name: String,
    app_profile_id: String,
}

impl BigtableDirectoryService {
    pub async fn connect(
        project_id: &str,
        instance_name: &str,
        table_name: String,
        family_name: String,
        is_read_only: bool,
        channel_size: usize,
        timeout: Option<std::time::Duration>,
    ) -> Result<Self, bigtable::Error> {
        let connection = bigtable::BigTableConnection::new(
            project_id,
            instance_name,
            is_read_only,
            channel_size,
            timeout,
        )
        .await?;

        Ok(Self {
            client: connection.client(),
            table_name,
            family_name,
            app_profile_id: "default".into(), // TODO
        })
    }

    /// Constructs Self from a already pre-initialized BigTable client.
    /// Used for tests.
    #[cfg(test)]
    pub fn from_client(
        client: bigtable::BigTable,
        table_name: String,
        family_name: String,
        app_profile_id: String,
    ) -> Self {
        Self {
            client,
            table_name,
            family_name,
            app_profile_id,
        }
    }
}

/// Derives the row/column key for a given blake3 digest.
/// We use hexlower encoding, also because it can't be misinterpreted as RE2.
fn derive_directory_key(digest: &B3Digest) -> String {
    HEXLOWER.encode(digest.as_slice())
}

#[async_trait]
impl DirectoryService for BigtableDirectoryService {
    #[instrument(skip(self, digest), err, fields(directory.digest = %digest))]
    async fn get(&self, digest: &B3Digest) -> Result<Option<proto::Directory>, Error> {
        let mut client = self.client.clone();
        let directory_key = derive_directory_key(digest);

        let request = bigtable_v2::ReadRowsRequest {
            app_profile_id: self.app_profile_id.clone(),
            table_name: client.get_full_table_name(&self.table_name),
            rows_limit: 1,
            rows: Some(bigtable_v2::RowSet {
                row_keys: vec![directory_key.clone().into()],
                row_ranges: vec![],
            }),
            // filter out all but the cell with a qualifier matching our digest.
            // This is to ensure we don't fail once we start bucketing.
            filter: Some(bigtable_v2::RowFilter {
                filter: Some(bigtable_v2::row_filter::Filter::ColumnQualifierRegexFilter(
                    directory_key.clone().into(),
                )),
            }),
            ..Default::default()
        };

        let mut response = client
            .read_rows(request)
            .await
            .map_err(|e| Error::StorageError(format!("unable to read rows: {}", e)))?;

        if response.len() != 1 {
            if response.len() > 1 {
                // This shouldn't happen, we limit number of rows to 1
                return Err(Error::StorageError(
                    "got more than one row from bigtable".into(),
                ));
            }
            // else, this is simply a "not found".
            return Ok(None);
        }

        let (row_key, mut row_cells) = response.pop().unwrap();
        if row_key != directory_key.as_bytes() {
            // This shouldn't happen, we requested this row key.
            return Err(Error::StorageError(
                "got wrong row key from bigtable".into(),
            ));
        }

        if let Some(row_cell) = row_cells.pop() {
            // Ensure there's only one cell (so no more left after the pop())
            // This shouldn't happen, We filter out other cells in our query.
            if !row_cells.is_empty() {
                return Err(Error::StorageError(
                    "more than one cell returned from bigtable".into(),
                ));
            }

            // We also require the qualifier to be correct in the filter above,
            // so this shouldn't happen.
            if directory_key.as_bytes() != &row_cell.qualifier {
                return Err(Error::StorageError("unexpected cell qualifier".into()));
            }

            // For the data in that cell, ensure the digest matches what's requested, before parsing.
            let got_digest = B3Digest::from(blake3::hash(&row_cell.value).as_bytes());
            if got_digest != *digest {
                return Err(Error::StorageError(format!(
                    "invalid digest: {}",
                    got_digest
                )));
            }

            // Try to parse the value into a Directory message.
            let directory = proto::Directory::decode(Bytes::from(row_cell.value)).map_err(|e| {
                Error::StorageError(format!("unable to decode directory proto: {}", e))
            })?;

            // validate the Directory.
            directory
                .validate()
                .map_err(|e| Error::StorageError(format!("invalid Directory message: {}", e)))?;

            Ok(Some(directory))
        } else {
            return Err(Error::StorageError("found no cells".into()));
        }
    }

    #[instrument(skip(self, directory), err, fields(directory.digest = %directory.digest()))]
    async fn put(&self, directory: proto::Directory) -> Result<B3Digest, Error> {
        let directory_digest = directory.digest();
        let mut client = self.client.clone();
        let directory_key = derive_directory_key(&directory_digest);

        // Ensure the directory we're trying to upload passes validation
        directory
            .validate()
            .map_err(|e| Error::InvalidRequest(format!("directory is invalid: {}", e)))?;

        let data = directory.encode_to_vec();
        if data.len() as u64 > CELL_SIZE_LIMIT {
            return Err(Error::StorageError(
                "Directory exceeds cell limit on Bigtable".into(),
            ));
        }

        let resp = client
            .check_and_mutate_row(bigtable_v2::CheckAndMutateRowRequest {
                table_name: client.get_full_table_name(&self.table_name),
                app_profile_id: self.app_profile_id.clone(),
                row_key: directory_key.clone().into(),
                predicate_filter: Some(bigtable_v2::RowFilter {
                    filter: Some(bigtable_v2::row_filter::Filter::ColumnQualifierRegexFilter(
                        directory_key.clone().into(),
                    )),
                }),
                // If the column was already found, do nothing.
                true_mutations: vec![],
                // Else, do the insert.
                false_mutations: vec![
                    // https://cloud.google.com/bigtable/docs/writes
                    bigtable_v2::Mutation {
                        mutation: Some(bigtable_v2::mutation::Mutation::SetCell(
                            bigtable_v2::mutation::SetCell {
                                family_name: self.family_name.clone(),
                                column_qualifier: directory_key.clone().into(),
                                timestamp_micros: -1, // use server time to fill timestamp
                                value: data,
                            },
                        )),
                    },
                ],
            })
            .await
            .map_err(|e| Error::StorageError(format!("unable to mutate rows: {}", e)))?;

        if resp.predicate_matched {
            trace!("already existed")
        }

        Ok(directory_digest)
    }

    #[instrument(skip_all, fields(directory.digest = %root_directory_digest))]
    fn get_recursive(
        &self,
        root_directory_digest: &B3Digest,
    ) -> BoxStream<Result<proto::Directory, Error>> {
        traverse_directory(self.clone(), root_directory_digest)
    }

    #[instrument(skip_all)]
    fn put_multiple_start(&self) -> Box<(dyn DirectoryPutter + 'static)>
    where
        Self: Clone,
    {
        Box::new(SimplePutter::new(self.clone()))
    }
}
