use std::path::PathBuf;

use tempfile::TempDir;
use tonic::Request;

use crate::blobservice::{BlobService, SledBlobService};
use crate::chunkservice::{ChunkService, SledChunkService};
use crate::directoryservice::{DirectoryService, SledDirectoryService};
use crate::nar::NonCachingNARCalculationService;
use crate::pathinfoservice::{PathInfoService, SledPathInfoService};
use crate::proto::get_path_info_request::ByWhat::ByOutputHash;
use crate::proto::node::Node::Symlink;
use crate::proto::path_info_service_server::PathInfoService as GRPCPathInfoService;
use crate::proto::GRPCPathInfoServiceWrapper;
use crate::proto::PathInfo;
use crate::proto::{GetPathInfoRequest, Node, SymlinkNode};

use lazy_static::lazy_static;

lazy_static! {
    static ref DUMMY_OUTPUT_HASH: Vec<u8> = vec![
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00
    ];
}

// TODO: dedup these helpers
fn gen_pathinfo_service(p: PathBuf) -> impl PathInfoService {
    SledPathInfoService::new(p.join("pathinfo").to_path_buf()).unwrap()
}
fn gen_blob_service(p: PathBuf) -> impl BlobService {
    SledBlobService::new(p.join("blobs").to_path_buf()).unwrap()
}

fn gen_chunk_service(p: PathBuf) -> impl ChunkService + Clone {
    SledChunkService::new(p.join("chunks").to_path_buf()).unwrap()
}

fn gen_directory_service(p: PathBuf) -> impl DirectoryService {
    SledDirectoryService::new(p.join("directories").to_path_buf()).unwrap()
}

/// generates a GRPCPathInfoService out of blob, chunk, directory and pathinfo services.
///
/// It doesn't create underlying services on its own, as we don't need to
/// preseed them with existing data for the test - we only interact with it via
// the PathInfo GRPC interface.
/// It uses the NonCachingNARCalculationService NARCalculationService to
/// calculate NARs.
fn gen_grpc_service(p: PathBuf) -> impl GRPCPathInfoService {
    let blob_service = gen_blob_service(p.to_path_buf());
    let chunk_service = gen_chunk_service(p.to_path_buf());
    let directory_service = gen_directory_service(p.to_path_buf());
    let pathinfo_service = gen_pathinfo_service(p.to_path_buf());

    GRPCPathInfoServiceWrapper::new(
        pathinfo_service,
        NonCachingNARCalculationService::new(blob_service, chunk_service, directory_service),
    )
}

/// Trying to get a non-existent PathInfo should return a not found error.
#[tokio::test]
async fn not_found() -> anyhow::Result<()> {
    let tmpdir = TempDir::new()?;
    let service = gen_grpc_service(tmpdir.path().to_path_buf());

    let resp = service
        .get(Request::new(GetPathInfoRequest {
            by_what: Some(ByOutputHash(DUMMY_OUTPUT_HASH.to_vec())),
        }))
        .await;

    let resp = resp.expect_err("must fail");
    assert_eq!(resp.code(), tonic::Code::NotFound);

    Ok(())
}

/// Put a PathInfo into the store, get it back.
#[tokio::test]
async fn put_get() -> anyhow::Result<()> {
    let tmpdir = TempDir::new()?;
    let service = gen_grpc_service(tmpdir.path().to_path_buf());

    let path_info = PathInfo {
        node: Some(Node {
            node: Some(Symlink(SymlinkNode {
                name: "00000000000000000000000000000000-foo".to_string(),
                target: "doesntmatter".to_string(),
            })),
        }),
        ..Default::default()
    };

    let resp = service.put(Request::new(path_info.clone())).await;

    assert!(resp.is_ok());
    assert_eq!(resp.expect("must succeed").into_inner(), path_info);

    let resp = service
        .get(Request::new(GetPathInfoRequest {
            by_what: Some(ByOutputHash(DUMMY_OUTPUT_HASH.to_vec())),
        }))
        .await;

    assert!(resp.is_ok());
    assert_eq!(resp.expect("must succeed").into_inner(), path_info);

    Ok(())
}
