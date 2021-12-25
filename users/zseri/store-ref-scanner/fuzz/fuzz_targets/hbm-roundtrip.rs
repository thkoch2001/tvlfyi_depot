#![no_main]
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    use core::convert::TryInto;
    use store_ref_scanner::HalfBytesMask;
    for i in data.chunks_exact(16) {
        let a = HalfBytesMask(i.try_into().unwrap());
        let b = a.into_expanded();
        let c = HalfBytesMask::from_expanded(b);
        assert_eq!(a, c);
    }
});
