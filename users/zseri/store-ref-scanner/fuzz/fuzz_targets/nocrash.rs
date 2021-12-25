#![no_main]
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    use store_ref_scanner::{StoreRefScanner, StoreSpec};

    StoreRefScanner::new(&data[..], &StoreSpec::DFL_NIX2).count();
    StoreRefScanner::new(&data[..], &StoreSpec::DFL_YZIX1).count();
});
