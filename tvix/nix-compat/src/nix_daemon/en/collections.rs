use std::collections::BTreeMap;

use super::{NixSerialize, NixWrite};

impl<T: NixSerialize + Send> NixSerialize for Vec<T> {
    async fn serialize<W>(self, writer: &mut W) -> Result<(), W::Error>
    where
        W: ?Sized + NixWrite + Send,
    {
        writer.write_number(self.len() as u64).await?;
        for elem in self {
            writer.write(elem).await?;
        }
        Ok(())
    }
}

impl<K, V> NixSerialize for BTreeMap<K, V>
where
    K: NixSerialize + Ord + Send,
    V: NixSerialize + Send,
{
    async fn serialize<W>(self, writer: &mut W) -> Result<(), W::Error>
    where
        W: ?Sized + NixWrite + Send,
    {
        writer.write_number(self.len() as u64).await?;
        for (key, value) in self {
            writer.write(key).await?;
            writer.write(value).await?;
        }
        Ok(())
    }
}

impl<T: NixSerialize + Send> NixSerialize for Option<T> {
    async fn serialize<W>(self, writer: &mut W) -> Result<(), W::Error>
    where
        W: ?Sized + NixWrite + Send,
    {
        if let Some(v) = self {
            v.serialize(writer).await?
        }
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use std::collections::BTreeMap;
    use std::fmt;

    use hex_literal::hex;
    use rstest::rstest;
    use tokio_test::io::Builder;

    use crate::nix_daemon::en::writer::NixWriter;
    use crate::nix_daemon::en::{NixSerialize, NixWrite};
    use crate::ProtocolVersion;

    #[rstest]
    #[case::empty(vec![], &hex!("0000 0000 0000 0000"))]
    #[case::one(vec![0x29], &hex!("0100 0000 0000 0000 2900 0000 0000 0000"))]
    #[case::two(vec![0x7469, 10], &hex!("0200 0000 0000 0000 6974 0000 0000 0000 0A00 0000 0000 0000"))]
    #[tokio::test]
    async fn test_write_small_vec(#[case] expected: Vec<u64>, #[case] data: &[u8]) {
        let mock = Builder::new().write(data).build();
        let mut writer = NixWriter::new(mock, ProtocolVersion::from_parts(1, 37));
        writer.write(expected).await.unwrap();
    }

    fn empty_map() -> BTreeMap<u64, u64> {
        BTreeMap::new()
    }
    macro_rules! map {
        ($($key:expr => $value:expr),*) => {{
            let mut ret = BTreeMap::new();
            $(ret.insert($key, $value);)*
            ret
        }};
    }

    #[rstest]
    #[case::empty(empty_map(), &hex!("0000 0000 0000 0000"))]
    #[case::one(map![0x7469u64 => true], &hex!("0100 0000 0000 0000 6974 0000 0000 0000 0100 0000 0000 0000"))]
    #[tokio::test]
    async fn test_write_small_btree_map<E>(#[case] expected: E, #[case] data: &[u8])
    where
        E: NixSerialize + PartialEq + fmt::Debug + Send,
    {
        let mock = Builder::new().write(data).build();
        let mut writer = NixWriter::new(mock, ProtocolVersion::from_parts(1, 37));
        writer.write(expected).await.unwrap();
    }
}
