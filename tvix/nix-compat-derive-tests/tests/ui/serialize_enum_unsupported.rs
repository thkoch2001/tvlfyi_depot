use nix_compat_derive::NixSerialize;

#[derive(NixSerialize)]
pub enum Test {
    Foo { version: u8 },
}

fn main() {}
