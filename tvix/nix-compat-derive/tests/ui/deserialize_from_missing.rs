use nix_compat::NixDeserialize;

#[derive(NixDeserialize)]
#[nix(crate = "nix_compat", from = "u64")]
pub struct Test;

fn main() {}
