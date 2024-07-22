use nix_compat::NixDeserialize;

#[derive(NixDeserialize)]
#[nix(crate = "nix_compat")]
pub enum Test {
    #[nix(version = "..=10")]
    Old,
    #[nix(version = "15..=17")]
    Legacy,
    #[nix(version = "50..")]
    NewWay,
}

fn main() {}
