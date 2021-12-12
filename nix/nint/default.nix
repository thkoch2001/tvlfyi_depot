{ depot, pkgs, ... }:

let inherit (depot.nix.writers) rustSimpleBin;

in rustSimpleBin {
  name = "nint";
  dependencies = [ depot.third_party.rust-crates.serde_json ];
} (builtins.readFile ./nint.rs)
