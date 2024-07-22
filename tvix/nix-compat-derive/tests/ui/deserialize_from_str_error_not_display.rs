use std::str::FromStr;

use nix_compat::NixDeserialize;

#[derive(NixDeserialize)]
#[nix(crate = "nix_compat", from_str)]
pub struct Test;

impl FromStr for Test {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s == "test" {
            Ok(Test)
        } else {
            Err(())
        }
    }
}

fn main() {}
