use crate::nar::seekable::Reader;
use crate::tests::fixtures::blob_service_with_contents as blob_service;
use crate::tests::fixtures::directory_service_with_contents as directory_service;
use crate::tests::fixtures::*;
use rstest::*;
use rstest_reuse::*;
use std::io;
use std::sync::Arc;
use tokio::io::AsyncReadExt;
use tvix_castore::blobservice::BlobService;
use tvix_castore::directoryservice::DirectoryService;
use tvix_castore::Node;

#[apply(castore_fixtures_template)]
#[tokio::test]
async fn read_to_end(
    #[future] blob_service: Arc<dyn BlobService>,
    #[future] directory_service: Arc<dyn DirectoryService>,
    #[case] test_input: &Node,
    #[case] test_output: Result<Result<&Vec<u8>, io::ErrorKind>, crate::nar::RenderError>,
) {
    let reader_result = Reader::new(
        test_input.clone(),
        // don't put anything in the stores, as we don't actually do any requests.
        blob_service.await,
        directory_service.await,
    )
    .await;

    match (reader_result, test_output) {
        (Ok(_), Err(_)) => panic!("creating reader should have failed but succeeded"),
        (Err(err), Ok(_)) => panic!("creating reader should have succeeded but failed: {}", err),
        (Err(reader_err), Err(expected_err)) => {
            assert_eq!(format!("{}", reader_err), format!("{}", expected_err));
        }
        (Ok(mut reader), Ok(expected_read_result)) => {
            let mut buf: Vec<u8> = vec![];
            let read_result = reader.read_to_end(&mut buf).await;

            match (read_result, expected_read_result) {
                (Ok(_), Err(_)) => panic!("read_to_end should have failed but succeeded"),
                (Err(err), Ok(_)) => {
                    panic!("read_to_end should have succeeded but failed: {}", err)
                }
                (Err(read_err), Err(expected_read_err)) => {
                    assert_eq!(read_err.kind(), expected_read_err);
                }
                (Ok(_n), Ok(expected_read_result)) => {
                    assert_eq!(buf, expected_read_result.to_vec());
                }
            }
        }
    }
}

#[apply(castore_fixtures_template)]
#[tokio::test]
async fn seek(
    #[future] blob_service: Arc<dyn BlobService>,
    #[future] directory_service: Arc<dyn DirectoryService>,
    #[case] test_input: &Node,
    #[case] test_output: Result<Result<&Vec<u8>, io::ErrorKind>, crate::nar::RenderError>,
) {
    let reader_result = Reader::new(
        test_input.clone(),
        // don't put anything in the stores, as we don't actually do any requests.
        blob_service.await,
        directory_service.await,
    )
    .await;

    match (reader_result, test_output) {
        (Ok(_), Err(_)) => panic!("creating reader should have failed but succeeded"),
        (Err(err), Ok(_)) => panic!("creating reader should have succeeded but failed: {}", err),
        (Err(reader_err), Err(expected_err)) => {
            assert_eq!(format!("{}", reader_err), format!("{}", expected_err));
        }
        (Ok(mut reader), Ok(expected_read_result)) => {
            let mut buf: Vec<u8> = vec![];
            let read_result = reader.read_to_end(&mut buf).await;

            match (read_result, expected_read_result) {
                (Ok(_), Err(_)) => panic!("read_to_end should have failed but succeeded"),
                (Err(err), Ok(_)) => {
                    panic!("read_to_end should have succeeded but failed: {}", err)
                }
                (Err(read_err), Err(expected_read_err)) => {
                    assert_eq!(read_err.kind(), expected_read_err);
                }
                (Ok(_n), Ok(expected_read_result)) => {
                    assert_eq!(buf, expected_read_result.to_vec());
                }
            }
        }
    }
}
