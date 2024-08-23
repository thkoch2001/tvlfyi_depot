use crate::nar;

#[test]
fn symlink() {
    let mut buf = vec![];
    let node = nar::writer::open(&mut buf).unwrap();

    node.symlink(b"/nix/store/somewhereelse").unwrap();

    assert_eq!(include_bytes!("../tests/symlink.nar"), buf.as_slice());
}

#[cfg(feature = "async")]
#[tokio::test]
async fn symlink_async() {
    let mut buf = vec![];

    let node = nar::writer::r#async::open(&mut buf).await.unwrap();
    node.symlink(b"/nix/store/somewhereelse")
        .await
        .unwrap();

    assert_eq!(include_bytes!("../tests/symlink.nar"), buf.as_slice());
}

#[test]
fn file() {
    let mut buf = vec![];
    let node = nar::writer::open(&mut buf).unwrap();

    let file_contents = "Hello World!".to_string();
    node.file(
        false,
        file_contents.len() as u64,
        &mut std::io::Cursor::new(file_contents),
    )
    .unwrap();

    assert_eq!(include_bytes!("../tests/helloworld.nar"), buf.as_slice());
}

#[cfg(feature = "async")]
#[tokio::test]
async fn file_async() {
    use std::io::Cursor;

    let mut buf = vec![];

    let node = nar::writer::r#async::open(&mut buf).await.unwrap();

    let file_contents = "Hello World!".to_string();
    node.file(
        false,
        file_contents.len() as u64,
        &mut Cursor::new(file_contents),
    )
    .await
    .unwrap();

    assert_eq!(include_bytes!("../tests/helloworld.nar"), buf.as_slice());
}

#[test]
fn complicated() {
    let mut buf = vec![];
    let node = nar::writer::open(&mut buf).unwrap();

    let mut dir_node = node.directory().unwrap();

    let e = dir_node.entry(b".keep").unwrap();
    e.file(false, 0, &mut std::io::Cursor::new([]))
        .expect("read .keep must succeed");

    let e = dir_node.entry(b"aa").unwrap();
    e.symlink(b"/nix/store/somewhereelse")
        .expect("symlink must succeed");

    let e = dir_node.entry(b"keep").unwrap();
    let mut subdir_node = e.directory().expect("directory must succeed");

    let e_sub = subdir_node
        .entry(b".keep")
        .expect("subdir entry must succeed");
    e_sub.file(false, 0, &mut std::io::Cursor::new([])).unwrap();

    // close the subdir, and then the dir, which is required.
    subdir_node.close().unwrap();
    dir_node.close().unwrap();

    assert_eq!(include_bytes!("../tests/complicated.nar"), buf.as_slice());
}

#[cfg(feature = "async")]
#[tokio::test]
async fn complicated_async() {
    use std::io::Cursor;

    let mut buf = vec![];

    let node = nar::writer::r#async::open(&mut buf).await.unwrap();

    let mut dir_node = node.directory().await.unwrap();

    let e = dir_node.entry(b".keep").await.unwrap();
    e.file(false, 0, &mut Cursor::new([]))
        .await
        .expect("read .keep must succeed");

    let e = dir_node.entry(b"aa").await.unwrap();
    e.symlink(b"/nix/store/somewhereelse")
        .await
        .expect("symlink must succeed");

    let e = dir_node.entry(b"keep").await.unwrap();
    let mut subdir_node = e.directory().await.expect("directory must succeed");

    let e_sub = subdir_node
        .entry(b".keep")
        .await
        .expect("subdir entry must succeed");
    e_sub.file(false, 0, &mut Cursor::new([])).await.unwrap();

    // close the subdir, and then the dir, which is required.
    subdir_node.close().await.unwrap();
    dir_node.close().await.unwrap();

    assert_eq!(include_bytes!("../tests/complicated.nar"), buf.as_slice());
}
