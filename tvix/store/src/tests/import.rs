use super::utils::{gen_blob_service, gen_directory_service};
use crate::import::ingest_path;
use crate::proto;
use crate::tests::fixtures::DIRECTORY_COMPLICATED;
use crate::tests::fixtures::*;
use tempfile::TempDir;

#[cfg(target_family = "unix")]
use std::os::unix::ffi::OsStringExt;

#[cfg(target_family = "unix")]
#[test]
fn symlink() {
    let tmpdir = TempDir::new().unwrap();

    std::fs::create_dir_all(&tmpdir).unwrap();
    std::os::unix::fs::symlink(
        "/nix/store/somewhereelse",
        tmpdir.path().join("doesntmatter"),
    )
    .unwrap();

    let root_node = ingest_path(
        gen_blob_service(),
        gen_directory_service(),
        tmpdir.path().join("doesntmatter"),
    )
    .expect("must succeed");

    assert_eq!(
        crate::proto::node::Node::Symlink(proto::SymlinkNode {
            name: "doesntmatter".into(),
            target: "/nix/store/somewhereelse".into(),
        }),
        root_node,
    )
}

#[test]
fn single_file() {
    let tmpdir = TempDir::new().unwrap();

    std::fs::write(tmpdir.path().join("root"), HELLOWORLD_BLOB_CONTENTS).unwrap();

    let blob_service = gen_blob_service();

    let root_node = ingest_path(
        blob_service.clone(),
        gen_directory_service(),
        tmpdir.path().join("root"),
    )
    .expect("must succeed");

    assert_eq!(
        crate::proto::node::Node::File(proto::FileNode {
            name: "root".into(),
            digest: HELLOWORLD_BLOB_DIGEST.to_vec(),
            size: HELLOWORLD_BLOB_CONTENTS.len() as u32,
            executable: false,
        }),
        root_node,
    );

    // ensure the blob has been uploaded
    assert!(blob_service.has(&HELLOWORLD_BLOB_DIGEST).unwrap());
}

#[cfg(target_family = "unix")]
#[test]
fn complicated() {
    let tmpdir = TempDir::new().unwrap();

    // File ``.keep`
    std::fs::write(tmpdir.path().join(".keep"), vec![]).unwrap();
    // Symlink `aa`
    std::os::unix::fs::symlink("/nix/store/somewhereelse", tmpdir.path().join("aa")).unwrap();
    // Directory `keep`
    std::fs::create_dir(tmpdir.path().join("keep")).unwrap();
    // File ``keep/.keep`
    std::fs::write(tmpdir.path().join("keep").join(".keep"), vec![]).unwrap();

    let blob_service = gen_blob_service();
    let directory_service = gen_directory_service();

    let root_node = ingest_path(
        blob_service.clone(),
        directory_service.clone(),
        tmpdir.path(),
    )
    .expect("must succeed");

    // ensure root_node matched expectations
    assert_eq!(
        crate::proto::node::Node::Directory(proto::DirectoryNode {
            name: tmpdir.path().file_name().unwrap().to_os_string().into_vec(),
            digest: DIRECTORY_COMPLICATED.digest().to_vec(),
            size: DIRECTORY_COMPLICATED.size(),
        }),
        root_node,
    );

    // ensure DIRECTORY_WITH_KEEP and DIRECTORY_COMPLICATED have been uploaded
    assert!(directory_service
        .get(&DIRECTORY_WITH_KEEP.digest())
        .unwrap()
        .is_some());
    assert!(directory_service
        .get(&DIRECTORY_COMPLICATED.digest())
        .unwrap()
        .is_some());

    // ensure EMPTY_BLOB_CONTENTS has been uploaded
    assert!(blob_service.has(&EMPTY_BLOB_DIGEST).unwrap());
}
