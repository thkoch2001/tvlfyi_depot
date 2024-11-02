use super::{NixSerialize, NixWrite};

impl NixSerialize for u64 {
    async fn serialize<'a, W>(&'a self, writer: &'a mut W) -> Result<(), W::Error>
    where
        W: NixWrite + Send,
    {
        writer.write_number(*self).await
    }
}

impl NixSerialize for bool {
    async fn serialize<'a, W>(&'a self, writer: &'a mut W) -> Result<(), W::Error>
    where
        W: NixWrite + Send,
    {
        writer.write_number(if *self { 1 } else { 0 }).await
    }
}

impl NixSerialize for i64 {
    async fn serialize<'a, W>(&'a self, writer: &'a mut W) -> Result<(), W::Error>
    where
        W: NixWrite + Send,
    {
        writer.write_number(*self as u64).await
    }
}

impl NixSerialize for usize {
    async fn serialize<'a, W>(&'a self, writer: &'a mut W) -> Result<(), W::Error>
    where
        W: NixWrite + Send,
    {
        writer.write_number(*self as u64).await
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
    #[case::simple_false(false, &hex!("0000 0000 0000 0000"))]
    #[case::simple_true(true, &hex!("0100 0000 0000 0000"))]
    #[tokio::test]
    async fn test_write_bool(#[case] expected: bool, #[case] data: &[u8]) {
        let mock = Builder::new().write(data).build();

        let mut writer = NixWriter::new(mock, ProtocolVersion::from_parts(1, 37));
        writer.write(&expected).await.unwrap();
    }

    #[rstest]
    #[case::zero(0, &hex!("0000 0000 0000 0000"))]
    #[case::one(1, &hex!("0100 0000 0000 0000"))]
    #[case::other(0x563412, &hex!("1234 5600 0000 0000"))]
    #[case::max_value(u64::MAX, &hex!("FFFF FFFF FFFF FFFF"))]
    #[tokio::test]
    async fn test_write_u64(#[case] expected: u64, #[case] data: &[u8]) {
        let mock = Builder::new().write(data).build();
        let mut writer = NixWriter::new(mock, ProtocolVersion::from_parts(1, 37));
        writer.write(&expected).await.unwrap();
    }
}
