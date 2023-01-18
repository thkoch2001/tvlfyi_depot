use tempfile::TempDir;
use tokio::stream;
use tokio::sync::mpsc;
use tokio_stream::wrappers::ReceiverStream;
use tokio_stream::StreamExt;
use tonic::Request;
use tonic::Status;

use crate::proto::directory_service_server::DirectoryService;
use crate::proto::get_directory_request::ByWhat;
use crate::proto::Directory;
use crate::proto::GetDirectoryRequest;
use crate::proto::PutDirectoryResponse;
use crate::sled_directory_service::SledDirectoryService;
use lazy_static::lazy_static;

lazy_static! {
    static ref DUMMY_DIRECTORY_DIGEST: Vec<u8> = vec![
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00
    ];
}

// Send the specified GetDirectoryRequest.
// Returns an error in the case of an error response, or an error in one of the items in the stream,
// or a Vec<Directory> in the case of a successful request.
async fn get_directories<S: DirectoryService>(
    svc: &S,
    get_directory_request: GetDirectoryRequest,
) -> Result<Vec<Directory>, Status> {
    let resp = svc.get(tonic::Request::new(get_directory_request)).await;

    // if the response is an error itself, return the error, otherwise unpack
    let stream = match resp {
        Ok(resp) => resp,
        Err(status) => return Err(status),
    }
    .into_inner();

    let directory_results: Vec<Result<Directory, Status>> = stream.collect().await;

    // turn Vec<Result<Directory, Status> into Result<Vec<Directory>,Status>
    directory_results.into_iter().collect()
}

/// Trying to get a non-existent Directory should return a not found error.
#[tokio::test]
async fn not_found() -> anyhow::Result<()> {
    let service = SledDirectoryService::new(TempDir::new()?.path().to_path_buf())?;

    let resp = service
        .get(tonic::Request::new(GetDirectoryRequest {
            by_what: Some(ByWhat::Digest(DUMMY_DIRECTORY_DIGEST.to_vec())),
            ..Default::default()
        }))
        .await;

    let mut rx = resp.expect("must succeed").into_inner().into_inner();

    // The stream should contain one element, an error with Code::NotFound.
    let item = rx
        .recv()
        .await
        .expect("must be some")
        .expect_err("must be err");
    assert_eq!(item.code(), tonic::Code::NotFound);

    // … and nothing else
    assert!(rx.recv().await.is_none());

    Ok(())
}

// /// Put a Directory into the store, get it back.
#[tokio::test]
async fn put_get() -> anyhow::Result<()> {
    // upload the directory
    let service = SledDirectoryService::new(TempDir::new()?.path().to_path_buf())?;

    let mock_directory = Directory::default();

    let streaming_request = tonic_mock::streaming_request(vec![mock_directory.clone()]);
    let put_resp = service
        .put(streaming_request)
        .await
        .expect("must succeed")
        .into_inner();

    // the sent root_digest should match the calculated digest
    assert_eq!(put_resp.root_digest, mock_directory.digest());

    // get it back
    let items = get_directories(
        &service,
        GetDirectoryRequest {
            by_what: Some(ByWhat::Digest(mock_directory.digest().clone())),
            ..Default::default()
        },
    )
    .await
    .expect("must not error");

    assert_eq!(vec![mock_directory], items);

    Ok(())
}
