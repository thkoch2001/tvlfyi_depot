use bytes::Bytes;

use super::{NixSerialize, NixWrite};

impl NixSerialize for [u8] {
    async fn serialize<'a, W>(&'a self, writer: &'a mut W) -> Result<(), W::Error>
    where
        W: NixWrite + Send,
    {
        writer.write_bytes(self).await
    }
}

impl NixSerialize for str {
    async fn serialize<'a, W>(&'a self, writer: &'a mut W) -> Result<(), W::Error>
    where
        W: NixWrite + Send,
    {
        writer.write_bytes(self.as_bytes()).await?;
        Ok(())
    }
}

impl NixSerialize for String {
    async fn serialize<'a, W>(&'a self, writer: &'a mut W) -> Result<(), W::Error>
    where
        W: NixWrite + Send,
    {
        self.as_str().serialize(writer).await
    }
}

impl NixSerialize for Bytes {
    async fn serialize<'a, W>(&'a self, writer: &'a mut W) -> Result<(), W::Error>
    where
        W: NixWrite + Send,
    {
        self.as_ref().serialize(writer).await
    }
}

#[cfg(test)]
mod test {
    use hex_literal::hex;
    use rstest::rstest;
    use tokio_test::io::Builder;

    use crate::{
        nix_daemon::en::{writer::NixWriter, NixWrite},
        ProtocolVersion,
    };

    #[rstest]
    #[case::empty("", &hex!("0000 0000 0000 0000"))]
    #[case::one(")", &hex!("0100 0000 0000 0000 2900 0000 0000 0000"))]
    #[case::two("it", &hex!("0200 0000 0000 0000 6974 0000 0000 0000"))]
    #[case::three("tea", &hex!("0300 0000 0000 0000 7465 6100 0000 0000"))]
    #[case::four("were", &hex!("0400 0000 0000 0000 7765 7265 0000 0000"))]
    #[case::five("where", &hex!("0500 0000 0000 0000 7768 6572 6500 0000"))]
    #[case::six("unwrap", &hex!("0600 0000 0000 0000 756E 7772 6170 0000"))]
    #[case::seven("where's", &hex!("0700 0000 0000 0000 7768 6572 6527 7300"))]
    #[case::aligned("read_tea", &hex!("0800 0000 0000 0000 7265 6164 5F74 6561"))]
    #[case::more_bytes("read_tess", &hex!("0900 0000 0000 0000 7265 6164 5F74 6573 7300 0000 0000 0000"))]
    #[case::utf8("The quick brown 🦊 jumps over 13 lazy 🐶.", &hex!("2D00 0000 0000 0000  5468 6520 7175 6963  6b20 6272 6f77 6e20  f09f a68a 206a 756d  7073 206f 7665 7220  3133 206c 617a 7920  f09f 90b6 2e00 0000"))]
    #[tokio::test]
    async fn test_write_string(#[case] expected: String, #[case] data: &[u8]) {
        let mock = Builder::new().write(data).build();
        let mut writer = NixWriter::new(mock, ProtocolVersion::from_parts(1, 37));

        writer.write(&expected).await.unwrap();
    }
}
