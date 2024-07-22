use nix_compat::NixDeserialize;

#[derive(NixDeserialize)]
#[nix(crate = "nix_compat", try_from = "u64")]
pub struct Test;

impl TryFrom<u64> for Test {
    type Error = ();

    fn try_from(value: u64) -> Result<Test, Self::Error> {
        if value == 42 {
            Ok(Test)
        } else {
            Err(())
        }
    }
}

fn main() {}
