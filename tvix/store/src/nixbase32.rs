use data_encoding::Specification;
use lazy_static::lazy_static;

lazy_static! {
    static ref NIXBASE32: data_encoding::Encoding = nixbase32_encoding();
}

fn nixbase32_encoding() -> data_encoding::Encoding {
    let mut spec = Specification::new();
    spec.symbols.push_str("0123456789abcdfghijklmnpqrsvwxyz");

    spec.encoding().unwrap()
}

mod tests {
    #[test]
    fn decode() {
        let dec = crate::nixbase32::NIXBASE32
            .decode("00bgd045z0d4icpbc2yyz4gx48ak44la".as_bytes())
            .unwrap();

        assert_eq!(
            dec,
            [
                0x8a, 0x12, 0x32, 0x15, 0x22, 0xfd, 0x91, 0xef, 0xbd, 0x60, 0xeb, 0xb2, 0x48, 0x1a,
                0xf8, 0x85, 0x80, 0xf6, 0x16, 0x00,
            ]
        )
    }

    #[test]
    fn encode() {
        let enc = crate::nixbase32::NIXBASE32.encode(
            &[
                0x8a, 0x12, 0x32, 0x15, 0x22, 0xfd, 0x91, 0xef, 0xbd, 0x60, 0xeb, 0xb2, 0x48, 0x1a,
                0xf8, 0x85, 0x80, 0xf6, 0x16, 0x00,
            ][..],
        );

        assert_eq!(enc, "00bgd045z0d4icpbc2yyz4gx48ak44la");
    }
}
