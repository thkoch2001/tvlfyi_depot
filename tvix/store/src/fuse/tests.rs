use std::fs;
use std::io::Cursor;
use std::path::Path;
use std::sync::Arc;

use tempfile::TempDir;

use crate::blobservice::BlobService;
use crate::directoryservice::DirectoryService;
use crate::pathinfoservice::PathInfoService;
use crate::proto::{FileNode, PathInfo};
use crate::tests::fixtures;
use crate::tests::utils::{gen_blob_service, gen_directory_service, gen_pathinfo_service};
use crate::{proto, FUSE};

fn setup_and_mount<P: AsRef<Path>, F>(
    mountpoint: P,
    setup_fn: F,
) -> Result<fuser::BackgroundSession, std::io::Error>
where
    F: Fn(Arc<dyn BlobService>, Arc<dyn DirectoryService>, Arc<dyn PathInfoService>),
{
    let blob_service = gen_blob_service();
    let directory_service = gen_directory_service();
    let path_info_service = gen_pathinfo_service(blob_service.clone(), directory_service.clone());

    setup_fn(
        blob_service.clone(),
        directory_service.clone(),
        path_info_service.clone(),
    );

    let fs = FUSE::new(blob_service, directory_service, path_info_service);
    fuser::spawn_mount2(fs, mountpoint, &[])
}

fn populate_blob_a(
    blob_service: Arc<dyn BlobService>,
    _directory_service: Arc<dyn DirectoryService>,
    path_info_service: Arc<dyn PathInfoService>,
) {
    // Upload BLOB_A
    let mut bw = blob_service.open_write();
    std::io::copy(&mut Cursor::new(fixtures::BLOB_A.to_vec()), &mut bw)
        .expect("must succeed uploading");
    bw.close().expect("must succeed closing");

    // Create a PathInfo for it
    let path_info = PathInfo {
        node: Some(proto::Node {
            node: Some(proto::node::Node::File(FileNode {
                name: "00000000000000000000000000000000-test".to_string(),
                digest: fixtures::BLOB_A_DIGEST.to_vec(),
                size: fixtures::BLOB_A.len() as u32,
                executable: false,
            })),
        }),
        ..Default::default()
    };
    path_info_service.put(path_info).expect("must succeed");
}

/// Ensure mounting itself doesn't fail
#[test]
fn mount() {
    // https://plume.benboeckel.net/~/JustAnotherBlog/skipping-tests-in-rust
    if !std::path::Path::new("/dev/fuse").exists() {
        eprintln!("skipping test");
        return;
    }

    let tmpdir = TempDir::new().unwrap();

    let fuser_session = setup_and_mount(tmpdir.path(), |_, _, _| {}).expect("must succeed");

    fuser_session.join()
}

/// Ensure listing the root isn't allowed
#[test]
fn root() {
    // https://plume.benboeckel.net/~/JustAnotherBlog/skipping-tests-in-rust
    if !std::path::Path::new("/dev/fuse").exists() {
        eprintln!("skipping test");
        return;
    }
    let tmpdir = TempDir::new().unwrap();

    let fuser_session = setup_and_mount(tmpdir.path(), |_, _, _| {}).expect("must succeed");

    // read_dir succeeds, but getting the first element will fail.
    let mut it = fs::read_dir(tmpdir).expect("must succeed");

    let err = it.next().expect("must be some").expect_err("must be err");
    assert_eq!(std::io::ErrorKind::PermissionDenied, err.kind());

    // We need to drop `it`, so the filesystem can unmount itself and exit.
    drop(it);

    fuser_session.join()
}

/// Ensure we can stat a file at the root
#[test]
fn stat_file_at_root() {
    // https://plume.benboeckel.net/~/JustAnotherBlog/skipping-tests-in-rust
    if !std::path::Path::new("/dev/fuse").exists() {
        eprintln!("skipping test");
        return;
    }
    let tmpdir = TempDir::new().unwrap();

    let fuser_session = setup_and_mount(tmpdir.path(), populate_blob_a).expect("must succeed");

    let p = tmpdir.path().join("00000000000000000000000000000000-test");

    // peek at the file metadata
    let metadata = fs::metadata(p).expect("must succeed");

    assert!(metadata.is_file());
    assert!(metadata.permissions().readonly());
    assert_eq!(fixtures::BLOB_A.len() as u64, metadata.len());

    fuser_session.join()
}

/// Ensure we can read a file at the root
#[test]
fn read_file_at_root() {
    // https://plume.benboeckel.net/~/JustAnotherBlog/skipping-tests-in-rust
    if !std::path::Path::new("/dev/fuse").exists() {
        eprintln!("skipping test");
        return;
    }
    let tmpdir = TempDir::new().unwrap();

    let fuser_session = setup_and_mount(tmpdir.path(), populate_blob_a).expect("must succeed");

    let p = tmpdir.path().join("00000000000000000000000000000000-test");

    // peek at the file metadata
    let data = fs::read(p).expect("must succeed");

    // ensure size and contents match
    assert_eq!(fixtures::BLOB_A.len(), data.len());
    assert_eq!(fixtures::BLOB_A.to_vec(), data);

    fuser_session.join()
}

// TODO:
// - validate some more attributes?
// - ensure write doesn't work
// - symlink operations, what about read with nofollow?
// - complicated testcase:
//  - read and stat in there
//  - readdir at the root, at the children
