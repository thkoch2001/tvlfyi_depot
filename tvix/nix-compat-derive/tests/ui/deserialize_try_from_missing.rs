use nix_compat::NixDeserialize;

#[derive(NixDeserialize)]
#[nix(crate = "nix_compat", try_from = "u64")]
pub struct Test;

fn main() {}
