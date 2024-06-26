use crate::utils::AsyncIoBridge;

use super::{NarCalculationService, RenderError};
use count_write::CountWrite;
use nix_compat::nar::writer::r#async as nar_writer;
use sha2::{Digest, Sha256};
use tokio::io::{self, AsyncWrite, BufReader};
use tonic::async_trait;
use tracing::{instrument, Span};
use tracing_indicatif::span_ext::IndicatifSpanExt;
use tvix_castore::{
    blob::BlobReader,
    blobservice::BlobService,
    chunkstore::ChunkStore,
    directoryservice::DirectoryService,
    proto::{self as castorepb, NamedNode},
};

pub struct SimpleRenderer<CS, BS, DS> {
    chunk_store: CS,
    blob_service: BS,
    directory_service: DS,
}

impl<CS, BS, DS> SimpleRenderer<CS, BS, DS> {
    pub fn new(chunk_store: CS, blob_service: BS, directory_service: DS) -> Self {
        Self {
            chunk_store,
            blob_service,
            directory_service,
        }
    }
}

#[async_trait]
impl<CS, BS, DS> NarCalculationService for SimpleRenderer<CS, BS, DS>
where
    CS: ChunkStore + Clone + 'static,
    BS: BlobService + Clone + 'static,
    DS: DirectoryService + Clone,
{
    async fn calculate_nar(
        &self,
        root_node: &castorepb::node::Node,
    ) -> Result<(u64, [u8; 32]), tvix_castore::Error> {
        calculate_size_and_sha256(
            root_node,
            self.chunk_store.clone(),
            self.blob_service.clone(),
            self.directory_service.clone(),
        )
        .await
        .map_err(|e| tvix_castore::Error::StorageError(format!("failed rendering nar: {}", e)))
    }
}

/// Invoke [write_nar], and return the size and sha256 digest of the produced
/// NAR output.
#[instrument(skip_all, fields(indicatif.pb_show=1))]
pub async fn calculate_size_and_sha256<CS, BS, DS>(
    root_node: &castorepb::node::Node,
    chunk_store: CS,
    blob_service: BS,
    directory_service: DS,
) -> Result<(u64, [u8; 32]), RenderError>
where
    CS: ChunkStore + Clone + 'static,
    BS: BlobService + Clone + 'static,
    DS: DirectoryService,
{
    let mut h = Sha256::new();
    let mut cw = CountWrite::from(&mut h);

    let span = Span::current();
    span.pb_set_message("Calculating NAR");
    span.pb_start();

    write_nar(
        // The hasher doesn't speak async. It doesn't
        // actually do any I/O, so it's fine to wrap.
        AsyncIoBridge(&mut cw),
        root_node,
        chunk_store,
        blob_service,
        directory_service,
    )
    .await?;

    Ok((cw.count(), h.finalize().into()))
}

/// Accepts a [castorepb::node::Node] pointing to the root of a (store) path,
/// and uses the passed blob_service and directory_service to perform the
/// necessary lookups as it traverses the structure.
/// The contents in NAR serialization are writen to the passed [AsyncWrite].
pub async fn write_nar<W, CS, BS, DS>(
    mut w: W,
    proto_root_node: &castorepb::node::Node,
    chunk_store: CS,
    blob_service: BS,
    directory_service: DS,
) -> Result<(), RenderError>
where
    W: AsyncWrite + Unpin + Send,
    CS: ChunkStore + Clone + 'static,
    BS: BlobService + Clone + 'static,
    DS: DirectoryService,
{
    // Initialize NAR writer
    let nar_root_node = nar_writer::open(&mut w)
        .await
        .map_err(RenderError::NARWriterError)?;

    walk_node(
        nar_root_node,
        proto_root_node,
        chunk_store,
        blob_service,
        directory_service,
    )
    .await?;

    Ok(())
}

/// Process an intermediate node in the structure.
/// This consumes the node.
async fn walk_node<CS, BS, DS>(
    nar_node: nar_writer::Node<'_, '_>,
    proto_node: &castorepb::node::Node,
    chunk_store: CS,
    blob_service: BS,
    directory_service: DS,
) -> Result<(CS, BS, DS), RenderError>
where
    CS: ChunkStore + Clone + 'static,
    BS: BlobService + Clone + 'static,
    DS: DirectoryService + Send,
{
    match proto_node {
        castorepb::node::Node::Symlink(proto_symlink_node) => {
            nar_node
                .symlink(&proto_symlink_node.target)
                .await
                .map_err(RenderError::NARWriterError)?;
        }
        castorepb::node::Node::File(proto_file_node) => {
            let digest_len = proto_file_node.digest.len();
            let digest = proto_file_node.digest.clone().try_into().map_err(|_| {
                RenderError::StoreError(io::Error::new(
                    io::ErrorKind::Other,
                    format!("invalid digest len {} in file node", digest_len),
                ))
            })?;

            let mut blob_reader =
                match BlobReader::new(&digest, chunk_store.clone(), blob_service.clone())
                    .await
                    .map_err(RenderError::StoreError)?
                {
                    Some(blob_reader) => Ok(BufReader::new(blob_reader)),
                    None => Err(RenderError::NARWriterError(io::Error::new(
                        io::ErrorKind::NotFound,
                        format!("blob with digest {} not found", &digest),
                    ))),
                }?;

            nar_node
                .file(
                    proto_file_node.executable,
                    proto_file_node.size,
                    &mut blob_reader,
                )
                .await
                .map_err(RenderError::NARWriterError)?;
        }
        castorepb::node::Node::Directory(proto_directory_node) => {
            let digest_len = proto_directory_node.digest.len();
            let digest = proto_directory_node
                .digest
                .clone()
                .try_into()
                .map_err(|_| {
                    RenderError::StoreError(io::Error::new(
                        io::ErrorKind::InvalidData,
                        format!("invalid digest len {} in directory node", digest_len),
                    ))
                })?;

            // look it up with the directory service
            match directory_service
                .get(&digest)
                .await
                .map_err(|e| RenderError::StoreError(e.into()))?
            {
                // if it's None, that's an error!
                None => Err(RenderError::DirectoryNotFound(
                    digest,
                    proto_directory_node.name.clone(),
                ))?,
                Some(proto_directory) => {
                    // start a directory node
                    let mut nar_node_directory = nar_node
                        .directory()
                        .await
                        .map_err(RenderError::NARWriterError)?;

                    // We put blob_service, directory_service back here whenever we come up from
                    // the recursion.
                    let mut chunk_store = chunk_store;
                    let mut blob_service = blob_service;
                    let mut directory_service = directory_service;

                    // for each node in the directory, create a new entry with its name,
                    // and then recurse on that entry.
                    for proto_node in proto_directory.nodes() {
                        let child_node = nar_node_directory
                            .entry(proto_node.get_name())
                            .await
                            .map_err(RenderError::NARWriterError)?;

                        (chunk_store, blob_service, directory_service) = Box::pin(walk_node(
                            child_node,
                            &proto_node,
                            chunk_store,
                            blob_service,
                            directory_service,
                        ))
                        .await?;
                    }

                    // close the directory
                    nar_node_directory
                        .close()
                        .await
                        .map_err(RenderError::NARWriterError)?;

                    return Ok((chunk_store, blob_service, directory_service));
                }
            }
        }
    }

    Ok((chunk_store, blob_service, directory_service))
}
