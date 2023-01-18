use tempfile::TempDir;

use crate::proto::directory_service_server::DirectoryService;
use crate::proto::get_directory_request::ByWhat;
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
// #[tokio::test]
// async fn put_get() -> anyhow::Result<()> {
//     let service = SledDirectoryService::new(TempDir::new()?.path().to_path_buf())?;

//     let resp = service.put();
//     Ok(())
// }
