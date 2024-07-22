use nix_compat::NixDeserialize;

#[derive(NixDeserialize)]
#[nix(crate = "nix_compat")]
pub struct Value(String);

#[derive(NixDeserialize)]
#[nix(crate = "nix_compat")]
pub struct Test {
    #[nix(version = "20..", default = "Value::make_default")]
    version: Value,
}

fn main() {}
