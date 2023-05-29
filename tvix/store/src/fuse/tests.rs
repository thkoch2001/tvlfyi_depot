use std::fs;
use std::io::Cursor;
use std::os::unix::prelude::MetadataExt;
use std::path::Path;
use std::sync::Arc;

use tempfile::TempDir;

use crate::blobservice::BlobService;
use crate::directoryservice::DirectoryService;
use crate::pathinfoservice::PathInfoService;
use crate::proto::{DirectoryNode, FileNode, PathInfo};
use crate::tests::fixtures;
use crate::tests::utils::{gen_blob_service, gen_directory_service, gen_pathinfo_service};
use crate::{proto, FUSE};

const BLOB_A_NAME: &str = "00000000000000000000000000000000-test";
const SYMLINK_NAME: &str = "11111111111111111111111111111111-test";
const DIRECTORY_WITH_KEEP_NAME: &str = "22222222222222222222222222222222-test";
const DIRECTORY_COMPLICATED_NAME: &str = "33333333333333333333333333333333-test";

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
                name: BLOB_A_NAME.to_string(),
                digest: fixtures::BLOB_A_DIGEST.to_vec(),
                size: fixtures::BLOB_A.len() as u32,
                executable: false,
            })),
        }),
        ..Default::default()
    };
    path_info_service.put(path_info).expect("must succeed");
}

fn populate_symlink(
    _blob_service: Arc<dyn BlobService>,
    _directory_service: Arc<dyn DirectoryService>,
    path_info_service: Arc<dyn PathInfoService>,
) {
    // Create a PathInfo for it
    let path_info = PathInfo {
        node: Some(proto::Node {
            node: Some(proto::node::Node::Symlink(proto::SymlinkNode {
                name: SYMLINK_NAME.to_string(),
                target: BLOB_A_NAME.to_string(),
            })),
        }),
        ..Default::default()
    };
    path_info_service.put(path_info).expect("must succeed");
}

fn populate_directory_with_keep(
    blob_service: Arc<dyn BlobService>,
    directory_service: Arc<dyn DirectoryService>,
    path_info_service: Arc<dyn PathInfoService>,
) {
    // upload empty blob
    let mut bw = blob_service.open_write();
    bw.close().expect("must succeed closing");

    // upload directory
    directory_service
        .put(fixtures::DIRECTORY_WITH_KEEP.clone())
        .expect("must succeed uploading");

    // upload pathinfo
    let path_info = PathInfo {
        node: Some(proto::Node {
            node: Some(proto::node::Node::Directory(DirectoryNode {
                name: DIRECTORY_WITH_KEEP_NAME.to_string(),
                digest: fixtures::DIRECTORY_WITH_KEEP.digest().to_vec(),
                size: fixtures::DIRECTORY_WITH_KEEP.size(),
            })),
        }),
        ..Default::default()
    };
    path_info_service.put(path_info).expect("must succeed");
}

fn populate_directory_complicated(
    blob_service: Arc<dyn BlobService>,
    directory_service: Arc<dyn DirectoryService>,
    path_info_service: Arc<dyn PathInfoService>,
) {
    // upload empty blob
    let mut bw = blob_service.open_write();
    bw.close().expect("must succeed closing");

    // upload inner directory
    directory_service
        .put(fixtures::DIRECTORY_WITH_KEEP.clone())
        .expect("must succeed uploading");

    // uplodad parent directory
    directory_service
        .put(fixtures::DIRECTORY_COMPLICATED.clone())
        .expect("must succeed uploading");

    // upload pathinfo
    let path_info = PathInfo {
        node: Some(proto::Node {
            node: Some(proto::node::Node::Directory(DirectoryNode {
                name: DIRECTORY_COMPLICATED_NAME.to_string(),
                digest: fixtures::DIRECTORY_COMPLICATED.digest().to_vec(),
                size: fixtures::DIRECTORY_COMPLICATED.size(),
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

    let p = tmpdir.path().join(BLOB_A_NAME);

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

    let p = tmpdir.path().join(BLOB_A_NAME);

    // read the file contents
    let data = fs::read(p).expect("must succeed");

    // ensure size and contents match
    assert_eq!(fixtures::BLOB_A.len(), data.len());
    assert_eq!(fixtures::BLOB_A.to_vec(), data);

    fuser_session.join()
}

/// Read the target of a symlink
#[test]
fn symlink_readlink() {
    // https://plume.benboeckel.net/~/JustAnotherBlog/skipping-tests-in-rust
    if !std::path::Path::new("/dev/fuse").exists() {
        eprintln!("skipping test");
        return;
    }
    let tmpdir = TempDir::new().unwrap();

    let fuser_session = setup_and_mount(tmpdir.path(), populate_symlink).expect("must succeed");
    let p = tmpdir.path().join(SYMLINK_NAME);

    let target = fs::read_link(&p).expect("must succeed");
    assert_eq!(BLOB_A_NAME, target.to_str().unwrap());

    // peek at the file metadata, which follows symlinks.
    // this must fail, as we didn't populate the target.
    let e = fs::metadata(&p).expect_err("must fail");
    assert_eq!(std::io::ErrorKind::NotFound, e.kind());

    // peeking at the file metadata without following symlinks will succeed.
    let metadata = fs::symlink_metadata(&p).expect("must succeed");
    assert!(metadata.is_symlink());

    // reading from the symlink (which follows) will fail, because the target doesn't exist.
    let e = fs::read(p).expect_err("must fail");
    assert_eq!(std::io::ErrorKind::NotFound, e.kind());

    fuser_session.join()
}

/// Read and stat a regular file through a symlink pointing to it.
#[test]
fn read_stat_through_symlink() {
    // https://plume.benboeckel.net/~/JustAnotherBlog/skipping-tests-in-rust
    if !std::path::Path::new("/dev/fuse").exists() {
        eprintln!("skipping test");
        return;
    }
    let tmpdir = TempDir::new().unwrap();

    let fuser_session = setup_and_mount(tmpdir.path(), |bs: Arc<_>, ds: Arc<_>, ps: Arc<_>| {
        populate_blob_a(bs.clone(), ds.clone(), ps.clone());
        populate_symlink(bs, ds, ps);
    })
    .expect("must succeed");

    let p_symlink = tmpdir.path().join(SYMLINK_NAME);
    let p_blob = tmpdir.path().join(SYMLINK_NAME);

    // peek at the file metadata, which follows symlinks.
    // this must now return the same metadata as when statting at the target directly.
    let metadata_symlink = fs::metadata(&p_symlink).expect("must succeed");
    let metadata_blob = fs::metadata(&p_blob).expect("must succeed");
    assert_eq!(metadata_blob.file_type(), metadata_symlink.file_type());
    assert_eq!(metadata_blob.len(), metadata_symlink.len());

    // reading from the symlink (which follows) will return the same data as if
    // we were reading from the file directly.
    assert_eq!(
        std::fs::read(p_blob).expect("must succeed"),
        std::fs::read(p_symlink).expect("must succeed"),
    );

    fuser_session.join()
}

/// Read a directory in the root, and validate some attributes.
#[test]
fn read_stat_directory() {
    // https://plume.benboeckel.net/~/JustAnotherBlog/skipping-tests-in-rust
    if !std::path::Path::new("/dev/fuse").exists() {
        eprintln!("skipping test");
        return;
    }
    let tmpdir = TempDir::new().unwrap();

    let fuser_session =
        setup_and_mount(tmpdir.path(), populate_directory_with_keep).expect("must succeed");

    let p = tmpdir.path().join(DIRECTORY_WITH_KEEP_NAME);

    // peek at the metadata of the directory
    let metadata = fs::metadata(&p).expect("must succeed");
    assert!(metadata.is_dir());
    assert!(metadata.permissions().readonly());

    fuser_session.join()
}

#[test]
/// Read a blob inside a directory. This ensures we successfully populate directory data.
fn read_blob_inside_dir() {
    // https://plume.benboeckel.net/~/JustAnotherBlog/skipping-tests-in-rust
    if !std::path::Path::new("/dev/fuse").exists() {
        eprintln!("skipping test");
        return;
    }
    let tmpdir = TempDir::new().unwrap();

    let fuser_session =
        setup_and_mount(tmpdir.path(), populate_directory_with_keep).expect("must succeed");

    let p = tmpdir.path().join(DIRECTORY_WITH_KEEP_NAME).join(".keep");

    // peek at metadata.
    let metadata = fs::metadata(&p).expect("must succeed");
    assert!(metadata.is_file());
    assert!(metadata.permissions().readonly());

    // read from it
    let data = fs::read(&p).expect("must succeed");
    assert_eq!(fixtures::EMPTY_BLOB_CONTENTS.to_vec(), data);

    fuser_session.join()
}

#[test]
/// Read a blob inside a directory inside a directory. This ensures we properly
/// populate directories as we traverse down the structure.
fn read_blob_deep_inside_dir() {
    // https://plume.benboeckel.net/~/JustAnotherBlog/skipping-tests-in-rust
    if !std::path::Path::new("/dev/fuse").exists() {
        eprintln!("skipping test");
        return;
    }
    let tmpdir = TempDir::new().unwrap();

    let fuser_session =
        setup_and_mount(tmpdir.path(), populate_directory_complicated).expect("must succeed");

    let p = tmpdir
        .path()
        .join(DIRECTORY_COMPLICATED_NAME)
        .join("keep")
        .join(".keep");

    // peek at metadata.
    let metadata = fs::metadata(&p).expect("must succeed");
    assert!(metadata.is_file());
    assert!(metadata.permissions().readonly());

    // read from it
    let data = fs::read(&p).expect("must succeed");
    assert_eq!(fixtures::EMPTY_BLOB_CONTENTS.to_vec(), data);

    fuser_session.join()
}

#[test]
/// Ensure we allocate the same inodes for the same directory contents.
/// $DIRECTORY_COMPLICATED_NAME/keep contains the same data as $DIRECTORY_WITH_KEEP.
fn compare_inodes_directories() {
    // https://plume.benboeckel.net/~/JustAnotherBlog/skipping-tests-in-rust
    if !std::path::Path::new("/dev/fuse").exists() {
        eprintln!("skipping test");
        return;
    }
    let tmpdir = TempDir::new().unwrap();

    let fuser_session = setup_and_mount(tmpdir.path(), |bs: Arc<_>, ds: Arc<_>, ps: Arc<_>| {
        populate_directory_with_keep(bs.clone(), ds.clone(), ps.clone());
        populate_directory_complicated(bs, ds, ps);
    })
    .expect("must succeed");

    let p_dir_with_keep = tmpdir.path().join(DIRECTORY_WITH_KEEP_NAME);
    let p_sibling_dir = tmpdir.path().join(DIRECTORY_COMPLICATED_NAME).join("keep");

    // peek at metadata.
    assert_eq!(
        fs::metadata(&p_dir_with_keep).expect("must succeed").ino(),
        fs::metadata(&p_sibling_dir).expect("must succeed").ino()
    );

    fuser_session.join()
}

// TODO: ensure $DIRECTORY_COMPLICATED_NAME/keep/.keep and DIRECTORY_COMPLICATED_NAME/.keep got the same inodes.
// TODO: construct a second symlink, pointing to the same target as in DIRECTORY_COMPLICATED and test that too

/// Check we match paths exactly.
#[test]
fn read_wrong_paths_in_root() {
    // https://plume.benboeckel.net/~/JustAnotherBlog/skipping-tests-in-rust
    if !std::path::Path::new("/dev/fuse").exists() {
        eprintln!("skipping test");
        return;
    }
    let tmpdir = TempDir::new().unwrap();

    let fuser_session = setup_and_mount(tmpdir.path(), populate_blob_a).expect("must succeed");

    // wrong name
    assert!(!tmpdir
        .path()
        .join("00000000000000000000000000000000-tes")
        .exists());

    // invalid hash
    assert!(!tmpdir
        .path()
        .join("0000000000000000000000000000000-test")
        .exists());

    // right name, must exist
    assert!(tmpdir
        .path()
        .join("00000000000000000000000000000000-test")
        .exists());

    // now wrong name with right hash still may not exist
    assert!(!tmpdir
        .path()
        .join("00000000000000000000000000000000-tes")
        .exists());

    fuser_session.join()
}

/// Make sure writes are not allowed
#[test]
fn disallow_writes() {
    // https://plume.benboeckel.net/~/JustAnotherBlog/skipping-tests-in-rust
    if !std::path::Path::new("/dev/fuse").exists() {
        eprintln!("skipping test");
        return;
    }

    let tmpdir = TempDir::new().unwrap();

    let fuser_session = setup_and_mount(tmpdir.path(), |_, _, _| {}).expect("must succeed");

    let p = tmpdir.path().join(BLOB_A_NAME);
    let e = std::fs::File::create(&p).expect_err("must fail");

    assert_eq!(std::io::ErrorKind::Unsupported, e.kind());

    fuser_session.join()
}

// TODO:
// - validate some more attributes?
// - complicated testcase: readdir both in root and further down
// - Tests for error cases? What happens if we end up not getting a blob?
