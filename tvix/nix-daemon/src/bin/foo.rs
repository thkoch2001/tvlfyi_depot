use nix_compat::ProtocolVersion;
use nix_compat_derive::NixSerialize;

#[derive(Debug, NixSerialize)]
pub struct TraceLine {
    have_pos: u64,
    hint: String,
}

#[derive(NixSerialize)]
pub struct NixError {
    #[nix(version = "26..")]
    type_: &'static str,
    // #[nix(version = "26..")]
    // level: Verbosity,
    #[nix(version = "26..")]
    name: &'static str,

    msg: String,
    #[nix(version = "26..")]
    have_pos: u64,
    #[nix(version = "26..")]
    traces: Vec<TraceLine>,

    #[nix(version = "..26")]
    exit_status: u64,
}

fn main() {
    let ver = ProtocolVersion::from_parts(1, 28);
    let range = ..25;
    println!("{}", (26..).contains(&ver.minor()));
}