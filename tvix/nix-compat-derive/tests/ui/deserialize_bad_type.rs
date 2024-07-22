use nix_compat::NixDeserialize;

pub struct BadType;

#[derive(NixDeserialize)]
#[nix(crate = "nix_compat")]
pub struct Test {
    version: BadType,
}

fn main() {}
