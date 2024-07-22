use nix_compat::NixDeserialize;

#[derive(NixDeserialize)]
#[nix(crate = "nix_compat")]
pub struct Test {
    #[nix(default = 12)]
    version: u8,
}

fn main() {}
