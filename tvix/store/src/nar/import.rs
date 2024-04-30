use async_stream::try_stream;
use nix_compat::nar::reader::r#async::{
    self as nar_reader, DirReader, Entry as ReaderEntry, Node as ReaderNode,
};
use tokio::io::AsyncBufRead;
use tvix_castore::{
    blobservice::BlobService,
    directoryservice::DirectoryService,
    import::{ingest_entries, IngestionEntry, IngestionError},
    proto::node::Node,
    PathBuf,
};

/// Ingests the contents from a [AsyncRead] providing NAR into the tvix store,
/// interacting with a [BlobService] and [DirectoryService].
/// It returns the castore root node or an error.
pub async fn ingest_nar<R, BS, DS>(
    blob_service: BS,
    directory_service: DS,
    r: &mut R,
) -> Result<Node, IngestionError<Error>>
where
    R: AsyncBufRead + Send + Unpin,
    BS: BlobService + Clone,
    DS: AsRef<dyn DirectoryService>,
{
    // open the NAR for reading.
    // The NAR reader emits node in DFS preorder.
    let root_node = nar_reader::open(r).await.map_err(|e| Error::IO(e))?;

    // maintain a stack of all partially read dir_readers and their path.
    let mut stack: Vec<(PathBuf, DirReader)> = Vec::new();

    let entries = try_stream! {
        match root_node {
            nar_reader::Node::Symlink { target } => {
                yield IngestionEntry::Symlink {
                    path: PathBuf::default(),
                    target,
                }
            }
            nar_reader::Node::File { executable, mut reader } => {
                let (digest, size) = {
                    let mut blob_writer = blob_service.open_write().await;
                    let size = tokio::io::copy_buf(&mut reader, &mut blob_writer).await?;

                    (blob_writer.close().await?.into(), size)
                };

                yield IngestionEntry::Regular {
                    path: PathBuf::default(),
                    size,
                    executable,
                    digest
                }
            }
            nar_reader::Node::Directory(dir_reader) => {
                // push root dir to stack.
                stack.push((PathBuf::default(), dir_reader));
            }
        }


        loop {
            // check if there's still something on the stack
            let (parent_path, mut dir_reader) = match stack.pop() {
                Some((parent_path,dir_reader)) => (parent_path, dir_reader),
                None => break,
            };

            // consume entries from the dir_reader.
            loop {
                if let Some(ReaderEntry { name, node }) = dir_reader.next().await? {
                    let path = parent_path.try_join(&name).expect("Tvix bug: failed to join name");
                    match node {
                        ReaderNode::Directory(child_dir_reader) => {
                            // another child directory. push back what we popped from the stack, push the child, then continue.
                            stack.push((parent_path.clone(), dir_reader));
                            stack.push((path, child_dir_reader));
                        },
                        ReaderNode::Symlink { target } => {
                            yield IngestionEntry::Symlink { path, target: target };
                        },
                        ReaderNode::File {
                            executable, mut reader
                        } => {
                                let (digest, size) = {
                                    let mut blob_writer = blob_service.open_write().await;
                                    let size = tokio::io::copy_buf(&mut reader, &mut blob_writer).await?;

                                    (blob_writer.close().await?.into(), size)
                                };

                                yield IngestionEntry::Regular { path, size, executable, digest  }
                        }
                    }
                } else {
                    // we're done with this directory, emit the IngestionEntry for it.
                    yield IngestionEntry::Dir {
                        path: parent_path.clone(),
                    };
                }
            }

        }
    };

    Ok(ingest_entries(directory_service, Box::pin(entries)).await?)
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error(transparent)]
    IO(#[from] std::io::Error),
}

#[cfg(test)]
mod test {
    use crate::nar::ingest_nar;
    use std::io::Cursor;
    use std::sync::Arc;

    use rstest::*;
    use tokio_stream::StreamExt;
    use tvix_castore::blobservice::BlobService;
    use tvix_castore::directoryservice::DirectoryService;
    use tvix_castore::fixtures::{
        DIRECTORY_COMPLICATED, DIRECTORY_WITH_KEEP, EMPTY_BLOB_DIGEST, HELLOWORLD_BLOB_CONTENTS,
        HELLOWORLD_BLOB_DIGEST,
    };
    use tvix_castore::proto as castorepb;

    use crate::tests::fixtures::{
        blob_service, directory_service, NAR_CONTENTS_COMPLICATED, NAR_CONTENTS_HELLOWORLD,
        NAR_CONTENTS_SYMLINK,
    };

    #[rstest]
    #[tokio::test]
    async fn single_symlink(
        blob_service: Arc<dyn BlobService>,
        directory_service: Arc<dyn DirectoryService>,
    ) {
        let root_node = ingest_nar(
            blob_service,
            directory_service,
            &mut Cursor::new(&NAR_CONTENTS_SYMLINK.clone()),
        )
        .await
        .expect("must parse");

        assert_eq!(
            castorepb::node::Node::Symlink(castorepb::SymlinkNode {
                name: "".into(), // name must be empty
                target: "/nix/store/somewhereelse".into(),
            }),
            root_node
        );
    }

    #[rstest]
    #[tokio::test]
    async fn single_file(
        blob_service: Arc<dyn BlobService>,
        directory_service: Arc<dyn DirectoryService>,
    ) {
        let root_node = ingest_nar(
            blob_service.clone(),
            directory_service,
            &mut Cursor::new(&NAR_CONTENTS_HELLOWORLD.clone()),
        )
        .await
        .expect("must parse");

        assert_eq!(
            castorepb::node::Node::File(castorepb::FileNode {
                name: "".into(), // name must be empty
                digest: HELLOWORLD_BLOB_DIGEST.clone().into(),
                size: HELLOWORLD_BLOB_CONTENTS.len() as u64,
                executable: false,
            }),
            root_node
        );

        // blobservice must contain the blob
        assert!(blob_service.has(&HELLOWORLD_BLOB_DIGEST).await.unwrap());
    }

    #[rstest]
    #[tokio::test]
    async fn complicated(
        blob_service: Arc<dyn BlobService>,
        directory_service: Arc<dyn DirectoryService>,
    ) {
        let root_node = ingest_nar(
            blob_service.clone(),
            directory_service.clone(),
            &mut Cursor::new(&NAR_CONTENTS_COMPLICATED.clone()),
        )
        .await
        .expect("must parse");

        assert_eq!(
            castorepb::node::Node::Directory(castorepb::DirectoryNode {
                name: "".into(), // name must be empty
                digest: DIRECTORY_COMPLICATED.digest().into(),
                size: DIRECTORY_COMPLICATED.size(),
            }),
            root_node,
        );

        // blobservice must contain the blob
        assert!(blob_service.has(&EMPTY_BLOB_DIGEST).await.unwrap());

        // directoryservice must contain the directories, at least with get_recursive.
        let resp: Result<Vec<castorepb::Directory>, _> = directory_service
            .get_recursive(&DIRECTORY_COMPLICATED.digest())
            .collect()
            .await;

        let directories = resp.unwrap();

        assert_eq!(2, directories.len());
        assert_eq!(DIRECTORY_COMPLICATED.clone(), directories[0]);
        assert_eq!(DIRECTORY_WITH_KEEP.clone(), directories[1]);
    }
}
