//! This contains test scenarios that a given [PathInfoService] needs to pass.
//! We use [rstest] and [rstest_reuse] to provide all services we want to test
//! against, and then apply this template to all test functions.

// TODO: constructing pathinfoservices is hard, as we also need to construct blob and directory services

use std::sync::Arc;

use rstest::*;
use rstest_reuse::{self, *};
use tvix_castore::proto as castorepb;
use tvix_castore::{blobservice::BlobService, directoryservice::DirectoryService};

use crate::proto::PathInfo;
use crate::tests::fixtures::DUMMY_OUTPUT_HASH;

use super::PathInfoService;

/// Convenience type alias batching all three servives together.
#[allow(clippy::upper_case_acronyms)]
type BSDSPS = (
    Arc<dyn BlobService>,
    Arc<dyn DirectoryService>,
    Box<dyn PathInfoService>,
);

/// Creates a PathInfoService using a new Memory{Blob,Directory}Service.
pub async fn make_path_info_service(uri: &str) -> BSDSPS {
    let blob_service: Arc<dyn BlobService> = tvix_castore::blobservice::from_addr("memory://")
        .await
        .unwrap()
        .into();
    let directory_service: Arc<dyn DirectoryService> =
        tvix_castore::directoryservice::from_addr("memory://")
            .await
            .unwrap()
            .into();

    (
        blob_service.clone(),
        directory_service.clone(),
        crate::pathinfoservice::from_addr(uri, blob_service, directory_service)
            .await
            .unwrap(),
    )
}

#[template]
#[rstest]
#[case::memory(make_path_info_service("memory://").await)]
#[case::memory(make_path_info_service("sled://").await)]
pub fn path_info_services(
    #[case] services: (
        impl BlobService,
        impl DirectoryService,
        impl PathInfoService,
    ),
) {
}

/// Trying to get a non-existent PathInfo should return Ok(None).
#[apply(path_info_services)]
#[tokio::test]
async fn not_found(services: BSDSPS) {
    let (_, _, path_info_service) = services;
    assert!(path_info_service
        .get(*DUMMY_OUTPUT_HASH)
        .await
        .expect("must succeed")
        .is_none());
}

/// Put a PathInfo into the store, get it back.
#[apply(path_info_services)]
#[tokio::test]
async fn put_get(services: BSDSPS) {
    let (_, _, path_info_service) = services;

    // TODO: root node doesn't seem to have the right digest?!

    let path_info = PathInfo {
        node: Some(castorepb::Node {
            node: Some(castorepb::node::Node::Symlink(castorepb::SymlinkNode {
                name: "00000000000000000000000000000000-foo".into(),
                target: "doesntmatter".into(),
            })),
        }),
        ..Default::default()
    };

    // insert
    let resp = path_info_service
        .put(path_info.clone())
        .await
        .expect("must succeed");

    // expect the returned PathInfo to be equal (for now)
    assert_eq!(path_info, resp);

    // get it back
    let resp = path_info_service
        .get(*DUMMY_OUTPUT_HASH)
        .await
        .expect("must succeed");

    assert_eq!(Some(path_info), resp);
}
