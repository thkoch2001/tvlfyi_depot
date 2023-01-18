use tempfile::TempDir;

use crate::proto::directory_service_server::DirectoryService;
use crate::proto::get_directory_request::ByWhat;
use crate::proto::GetDirectoryRequest;
use crate::proto::PutDirectoryResponse;
use crate::sled_directory_service::SledDirectoryService;
use lazy_static::lazy_static;

lazy_static! {
    static ref DUMMY_OUTPUT_HASH: Vec<u8> = vec![
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00
    ];
}

/// Trying to get a non-existent Directory should return a not found error.
#[tokio::test]
async fn not_found() -> anyhow::Result<()> {
    let service = SledDirectoryService::new(TempDir::new()?.path().to_path_buf())?;

    let resp = service
        .get(tonic::Request::new(GetDirectoryRequest {
            by_what: Some(ByWhat::Digest(DUMMY_OUTPUT_HASH.to_vec())),
            ..Default::default()
        }))
        .await;

    let resp = resp.expect_err("must fail");
    assert_eq!(resp.code(), tonic::Code::NotFound);

    Ok(())
}

// /// Put a Directory into the store, get it back.
// #[tokio::test]
// async fn put_get() -> anyhow::Result<()> {
//     let service = SledDirectoryService::new(TempDir::new()?.path().to_path_buf())?;

//     let resp = service.put();
//     Ok(())
// }
