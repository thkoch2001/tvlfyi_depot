use std::io;

use test_case::test_case;

use crate::tests::fixtures;

use super::BlobService;
use super::MemoryBlobService;
use super::SledBlobService;

// TODO: avoid having to define all different services we test against for all functions.
// maybe something like rstest can be used?

fn gen_memory_blob_service() -> impl BlobService {
    MemoryBlobService::default()
}
fn gen_sled_blob_service() -> impl BlobService {
    SledBlobService::new_temporary().unwrap()
}

// TODO: add GRPC blob service here.

/// Using [BlobService::has] on a non-existing blob should return false
#[test_case(gen_memory_blob_service(); "memory")]
#[test_case(gen_sled_blob_service(); "sled")]
fn has_nonexistent_false(blob_service: impl BlobService) {
    assert_eq!(
        blob_service
            .has(&fixtures::BLOB_A_DIGEST)
            .expect("must not fail"),
        false
    );
}

/// Trying to read a non-existing blob should return a None instead of a reader.
#[test_case(gen_memory_blob_service(); "memory")]
#[test_case(gen_sled_blob_service(); "sled")]
fn not_found_read(blob_service: impl BlobService) {
    assert!(blob_service
        .open_read(&fixtures::BLOB_A_DIGEST)
        .expect("must not fail")
        .is_none())
}

/// Put a blob in the store, check has, get it back.
#[test_case(gen_memory_blob_service(); "memory")]
#[test_case(gen_sled_blob_service(); "sled")]
// TODO: parametrize the blob to use. we also want to test with a big blob,
// forcing things to be chunked up.
fn put_has_get(blob_service: impl BlobService) {
    let mut w = blob_service.open_write();

    let l = io::copy(&mut io::Cursor::new(fixtures::BLOB_A.to_vec()), &mut w)
        .expect("copy must succeed");
    assert_eq!(
        fixtures::BLOB_A.len(),
        l as usize,
        "written bytes must match blob length"
    );

    let digest = w.close().expect("close must succeed");

    assert_eq!(
        *fixtures::BLOB_A_DIGEST,
        digest,
        "returned digest must be correct"
    );

    // TODO: parametrize whether we do the `has` request or not
    assert_eq!(
        blob_service
            .has(&fixtures::BLOB_A_DIGEST)
            .expect("must not fail"),
        true,
        "blob service should now have the blob"
    );

    let mut r = blob_service
        .open_read(&fixtures::BLOB_A_DIGEST)
        .expect("open_read must succeed")
        .expect("must be some");

    let mut buf: Vec<u8> = Vec::new();
    let l = io::copy(&mut r, &mut buf).expect("copy must succeed");

    assert_eq!(
        fixtures::BLOB_A.len(),
        l as usize,
        "read bytes must match blob length"
    );

    assert_eq!(*fixtures::BLOB_A, buf, "read blob contents must match");
}
