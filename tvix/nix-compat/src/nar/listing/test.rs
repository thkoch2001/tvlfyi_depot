use crate::nar;

#[test]
fn nixos_release() {
    let listing_bytes = include_bytes!("../tests/nixos-release.ls");
    let listing: nar::listing::Listing = serde_json::from_slice(listing_bytes).unwrap();
}
