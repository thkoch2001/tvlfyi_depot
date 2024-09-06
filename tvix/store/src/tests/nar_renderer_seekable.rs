use crate::nar::seekable::Reader;
use crate::tests::fixtures::blob_service;
use crate::tests::fixtures::directory_service;
use crate::tests::fixtures::*;
use rstest::*;
use std::io;
use std::sync::Arc;
use tokio::io::sink;
use tokio::io::AsyncReadExt;
use tvix_castore::blobservice::BlobService;
use tvix_castore::directoryservice::DirectoryService;
use tvix_castore::Node;

#[rstest]
#[tokio::test]
async fn single_symlink(
    blob_service: Arc<dyn BlobService>,
    directory_service: Arc<dyn DirectoryService>,
) {
    let mut buf: Vec<u8> = vec![];

    let mut reader = Reader::new(
        Node::Symlink {
            target: "/nix/store/somewhereelse".try_into().unwrap(),
        },
        // don't put anything in the stores, as we don't actually do any requests.
        blob_service,
        directory_service,
    )
    .await
    .expect("must succeed");
    reader.read_to_end(&mut buf).await.expect("must succeed");

    assert_eq!(buf, NAR_CONTENTS_SYMLINK.to_vec());
}

/// Make sure the NARRenderer fails if a referred blob doesn't exist.
#[rstest]
#[tokio::test]
async fn single_file_missing_blob(
    blob_service: Arc<dyn BlobService>,
    directory_service: Arc<dyn DirectoryService>,
) {
    let mut reader = Reader::new(
        Node::File {
            digest: HELLOWORLD_BLOB_DIGEST.clone(),
            size: HELLOWORLD_BLOB_CONTENTS.len() as u64,
            executable: false,
        },
        // the blobservice is empty intentionally, to provoke the error.
        blob_service,
        directory_service,
    )
    .await
    .expect("must succeed");
    let e = tokio::io::copy(&mut reader, &mut sink())
        .await
        .expect_err("must fail");

    assert_eq!(io::ErrorKind::NotFound, e.kind());
}

/// Make sure the NAR Renderer fails if the returned blob meta has another size
/// than specified in the proto node.
#[rstest]
#[tokio::test]
async fn single_file_wrong_blob_size(
    blob_service: Arc<dyn BlobService>,
    directory_service: Arc<dyn DirectoryService>,
) {
    // insert blob into the store
    let mut writer = blob_service.open_write().await;
    tokio::io::copy(
        &mut io::Cursor::new(HELLOWORLD_BLOB_CONTENTS.to_vec()),
        &mut writer,
    )
    .await
    .unwrap();
    assert_eq!(
        HELLOWORLD_BLOB_DIGEST.clone(),
        writer.close().await.unwrap()
    );

    // Test with a root FileNode of a too big size
    let mut reader = Reader::new(
        Node::File {
            digest: HELLOWORLD_BLOB_DIGEST.clone(),
            size: 42, // <- note the wrong size here!
            executable: false,
        },
        blob_service.clone(),
        directory_service.clone(),
    )
    .await
    .expect("");

    let e = tokio::io::copy(&mut reader, &mut sink())
        .await
        .expect_err("must fail");

    match *e
        .into_inner()
        .expect("must have inner error")
        .downcast()
        .expect("must contain a RenderError")
    {
        crate::nar::RenderError::NARWriterError(e) => {
            assert_eq!(io::ErrorKind::UnexpectedEof, e.kind());
        }
        other => panic!("unexpected error: {:?}", other),
    }

    // Test with a root FileNode of a too small size
    let mut reader = Reader::new(
        Node::File {
            digest: HELLOWORLD_BLOB_DIGEST.clone(),
            size: 2, // <- note the wrong size here!
            executable: false,
        },
        blob_service,
        directory_service,
    )
    .await
    .expect("");

    let e = tokio::io::copy(&mut reader, &mut sink())
        .await
        .expect_err("must fail");

    match *e
        .into_inner()
        .expect("must have inner error")
        .downcast()
        .expect("must contain a RenderError")
    {
        crate::nar::RenderError::NARWriterError(e) => {
            assert_eq!(io::ErrorKind::InvalidInput, e.kind());
        }
        other => panic!("unexpected error: {:?}", other),
    }
}

#[rstest]
#[tokio::test]
async fn single_file(
    blob_service: Arc<dyn BlobService>,
    directory_service: Arc<dyn DirectoryService>,
) {
    // insert blob into the store
    let mut writer = blob_service.open_write().await;
    tokio::io::copy(&mut io::Cursor::new(HELLOWORLD_BLOB_CONTENTS), &mut writer)
        .await
        .unwrap();

    assert_eq!(
        HELLOWORLD_BLOB_DIGEST.clone(),
        writer.close().await.unwrap()
    );

    let mut buf: Vec<u8> = vec![];

    let mut reader = Reader::new(
        Node::File {
            digest: HELLOWORLD_BLOB_DIGEST.clone(),
            size: HELLOWORLD_BLOB_CONTENTS.len() as u64,
            executable: false,
        },
        blob_service,
        directory_service,
    )
    .await
    .expect("must succeed");

    reader.read_to_end(&mut buf).await.expect("must succeed");

    assert_eq!(buf, NAR_CONTENTS_HELLOWORLD.to_vec());
}

#[rstest]
#[tokio::test]
async fn test_complicated(
    blob_service: Arc<dyn BlobService>,
    directory_service: Arc<dyn DirectoryService>,
) {
    // put all data into the stores.
    // insert blob into the store
    let mut writer = blob_service.open_write().await;
    tokio::io::copy(&mut io::Cursor::new(EMPTY_BLOB_CONTENTS), &mut writer)
        .await
        .unwrap();
    assert_eq!(EMPTY_BLOB_DIGEST.clone(), writer.close().await.unwrap());

    // insert directories
    directory_service
        .put(DIRECTORY_WITH_KEEP.clone())
        .await
        .unwrap();
    directory_service
        .put(DIRECTORY_COMPLICATED.clone())
        .await
        .unwrap();

    let mut buf: Vec<u8> = vec![];

    let mut reader = Reader::new(
        Node::Directory {
            digest: DIRECTORY_COMPLICATED.digest(),
            size: DIRECTORY_COMPLICATED.size(),
        },
        blob_service.clone(),
        directory_service.clone(),
    )
    .await
    .expect("must succeed");

    reader.read_to_end(&mut buf).await.expect("must succeed");

    assert_eq!(buf, NAR_CONTENTS_COMPLICATED.to_vec());
}
