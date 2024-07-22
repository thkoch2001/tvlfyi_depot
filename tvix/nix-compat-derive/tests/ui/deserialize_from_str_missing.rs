use nix_compat::NixDeserialize;

#[derive(NixDeserialize)]
#[nix(crate = "nix_compat", from_str)]
pub struct Test;

fn main() {}
