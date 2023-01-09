use packed_simd::{m8x32, u8x32};

fn prefilter(haystack: u8x32) -> m8x32 {
    let alp = haystack.gt(u8x32::splat(b'a' - 1)) & haystack.lt(u8x32::splat(b'z' + 1));
    let num = haystack.gt(u8x32::splat(b'0' - 1)) & haystack.lt(u8x32::splat(b'9' + 1));
    alp | num
}

/// scan_clean returns `Err(&buffer[..n])` of known pointer-free data,
/// or `Ok(buffer)` if the entire buffer is pointer-free.
pub fn scan_clean(buffer: &[u8]) -> Result<&[u8], &[u8]> {
    let buffer = {
        let n = buffer.len() & !31;
        &buffer[..n]
    };

    let mut masks = buffer
        .chunks_exact(32)
        .map(|chunk| prefilter(u8x32::from_slice_unaligned(chunk)).bitmask())
        .enumerate()
        .map(|e| (e.0 * 32, e.1))
        .peekable();

    while let Some((offset, mask)) = masks.next() {
        let peek = masks.peek().map(|x| x.1).unwrap_or(!0 >> 1);
        let n = (!mask).leading_zeros() + (!peek).trailing_zeros();
        if n >= 32 {
            let offset = offset + mask.trailing_zeros() as usize;
            return Err(&buffer[..offset]);
        }
    }

    Ok(buffer)
}

#[cfg(test)]
mod test {
    #[test]
    fn scan_tail() {
        let buffer = b"_xfbmj7sl2ikicym9x3yq7cms5qx1w39k";
        assert_eq!(crate::scan_clean(buffer), Err(&buffer[..1]));
    }
    #[test]
    fn scan_straddle() {
        let buffer = b"________________xfbmj7sl2ikicym9x3yq7cms5qx1w39k________________";
        assert_eq!(crate::scan_clean(buffer), Err(&buffer[..16]));
    }
    #[test]
    fn scan_clean() {
        let buffer = b"x_______________xfbmj7sl2ikicym9x3yq-cms5qx1w3-k________________";
        assert_eq!(crate::scan_clean(buffer), Ok(&buffer[..]));
    }
}
