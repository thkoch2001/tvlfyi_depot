use hex_literal::hex;
use nix_compat::nix_daemon::en::NixWrite;
use nix_compat::{nix_daemon::en::writer::NixWriter, ProtocolVersion};
use nix_compat_derive::NixSerialize;

#[derive(Debug, PartialEq, Eq, NixSerialize)]
pub struct StructVersionTest {
    test: u64,
    #[nix(version = "20..")]
    hello: String,
}

#[derive(Debug, PartialEq, Eq, NixSerialize)]
pub enum EnumTest {
    Test1 {
        num: u64,
        #[nix(version = "20..")]
        s: String,
    },
    Test2(String),
}

#[tokio::test]
async fn write_struct_without_version() {
    let mock = tokio_test::io::Builder::new()
        .write(&hex!("5900 0000 0000 0000"))
        .build();
    let mut writer = NixWriter::new(mock, ProtocolVersion::from_parts(1, 19));

    writer
        .write(&StructVersionTest {
            test: 89,
            hello: "hello".into(),
        })
        .await
        .unwrap();
}

#[tokio::test]
async fn write_struct_with_version() {
    let mock = tokio_test::io::Builder::new()
        .write(&hex!(
            "5900 0000 0000 0000 0500 0000 0000 0000 6865 6C6C 6F00 0000"
        ))
        .build();
    let mut writer = NixWriter::new(mock, ProtocolVersion::from_parts(1, 20));

    writer
        .write(&StructVersionTest {
            test: 89,
            hello: "hello".into(),
        })
        .await
        .unwrap();
}

#[tokio::test]
async fn write_enum_1_with_version() {
    let mock = tokio_test::io::Builder::new()
        .write(&hex!(
            "7B00 0000 0000 0000 0500 0000 0000 0000 6B6C 6F6D 7000 0000"
        ))
        .build();
    let mut writer = NixWriter::new(mock, ProtocolVersion::from_parts(1, 20));

    writer
        .write(&EnumTest::Test1 {
            num: 123,
            s: "klomp".into(),
        })
        .await
        .unwrap();
}

#[tokio::test]
async fn write_enum_1() {
    let mock = tokio_test::io::Builder::new()
        .write(&hex!("7B00 0000 0000 0000"))
        .build();
    let mut writer = NixWriter::new(mock, ProtocolVersion::from_parts(1, 19));

    writer
        .write(&EnumTest::Test1 {
            num: 123,
            s: "klomp".into(),
        })
        .await
        .unwrap();
}

#[tokio::test]
async fn write_enum_2() {
    let mock = tokio_test::io::Builder::new()
        .write(&hex!("0500 0000 0000 0000 6B6C 6F6D 7000 0000"))
        .build();
    let mut writer = NixWriter::new(mock, ProtocolVersion::from_parts(1, 20));

    writer
        .write(&EnumTest::Test2("klomp".into()))
        .await
        .unwrap();
}
