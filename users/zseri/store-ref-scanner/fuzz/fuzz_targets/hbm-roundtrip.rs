#![no_main]
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: [u8; 16]| {
    use store_ref_scanner::HalfBytesMask;
    let a = HalfBytesMask(data);
    let b = a.into_expanded();
    let c = HalfBytesMask::from_expanded(b);
    assert_eq!(a, c);
});
