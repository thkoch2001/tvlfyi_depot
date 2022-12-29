use crate::{
    nixpath::{NixPath, ParseNixPathError},
    proto::{self, Node, PathInfo, ValidatePathInfoError},
};
use lazy_static::lazy_static;
use test_case::test_case;

lazy_static! {
    static ref DUMMY_DIGEST: Vec<u8> = vec![
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00,
    ];
}

const DUMMY_NAME: &str = "00000000000000000000000000000000-dummy";

#[test_case(
    None,
    Err(ValidatePathInfoError::NoNodePresent()) ;
    "No node"
)]
#[test_case(
    Some(Node { node: None }),
    Err(ValidatePathInfoError::NoNodePresent());
    "No node 2"
)]
fn validate_no_node(t_node: Option<proto::Node>, t_result: Result<NixPath, ValidatePathInfoError>) {
    // construct the PathInfo object
    let p = PathInfo {
        node: t_node,
        ..Default::default()
    };
    assert_eq!(t_result, p.validate());
}

#[test_case(
    proto::DirectoryNode {
        name: DUMMY_NAME.to_string(),
        digest: DUMMY_DIGEST.to_vec(),
        size: 0,
    },
    Ok(NixPath::from_string(DUMMY_NAME).expect("must succeed"));
    "ok"
)]
#[test_case(
    proto::DirectoryNode {
        name: DUMMY_NAME.to_string(),
        digest: vec![],
        size: 0,
    },
    Err(ValidatePathInfoError::InvalidDigestLen(0));
    "invalid digest length"
)]
#[test_case(
    proto::DirectoryNode {
        name: "invalid".to_string(),
        digest: DUMMY_DIGEST.to_vec(),
        size: 0,
    },
    Err(ValidatePathInfoError::InvalidNodeName(
        "invalid".to_string(),
        ParseNixPathError::InvalidName("".to_string())
    ));
    "invalid node name"
)]
fn validate_directory(
    t_directory_node: proto::DirectoryNode,
    t_result: Result<NixPath, ValidatePathInfoError>,
) {
    // construct the PathInfo object
    let p = PathInfo {
        node: Some(Node {
            node: Some(proto::node::Node::Directory(t_directory_node)),
        }),
        ..Default::default()
    };
    assert_eq!(t_result, p.validate());
}

#[test_case(
    proto::FileNode {
        name: DUMMY_NAME.to_string(),
        digest: DUMMY_DIGEST.to_vec(),
        size: 0,
        executable: false,
    },
    Ok(NixPath::from_string(DUMMY_NAME).expect("must succeed"));
    "ok"
)]
#[test_case(
    proto::FileNode {
        name: DUMMY_NAME.to_string(),
        digest: vec![],
        ..Default::default()
    },
    Err(ValidatePathInfoError::InvalidDigestLen(0));
    "invalid digest length"
)]
#[test_case(
    proto::FileNode {
        name: "invalid".to_string(),
        digest: DUMMY_DIGEST.to_vec(),
        ..Default::default()
    },
    Err(ValidatePathInfoError::InvalidNodeName(
        "invalid".to_string(),
        ParseNixPathError::InvalidName("".to_string())
    ));
    "invalid node name"
)]
fn validate_file(t_file_node: proto::FileNode, t_result: Result<NixPath, ValidatePathInfoError>) {
    // construct the PathInfo object
    let p = PathInfo {
        node: Some(Node {
            node: Some(proto::node::Node::File(t_file_node)),
        }),
        ..Default::default()
    };
    assert_eq!(t_result, p.validate());
}

#[test_case(
    proto::SymlinkNode {
        name: DUMMY_NAME.to_string(),
        ..Default::default()
    },
    Ok(NixPath::from_string(DUMMY_NAME).expect("must succeed"));
    "ok"
)]
#[test_case(
    proto::SymlinkNode {
        name: "invalid".to_string(),
        ..Default::default()
    },
    Err(ValidatePathInfoError::InvalidNodeName(
        "invalid".to_string(),
        ParseNixPathError::InvalidName("".to_string())
    ));
    "invalid node name"
)]
fn validate_symlink(
    t_symlink_node: proto::SymlinkNode,
    t_result: Result<NixPath, ValidatePathInfoError>,
) {
    // construct the PathInfo object
    let p = PathInfo {
        node: Some(Node {
            node: Some(proto::node::Node::Symlink(t_symlink_node)),
        }),
        ..Default::default()
    };
    assert_eq!(t_result, p.validate());
}
