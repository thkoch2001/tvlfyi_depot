{ pkgs, depot, ... }:

# Run crate2nix generate in the current working directory, then
# format the generated file with depotfmt.
pkgs.writeShellScriptBin "crate2nix-generate" ''
  ${pkgs.crate2nix}/bin/crate2nix generate --all-features
  ${depot.tools.depotfmt}/bin/depotfmt Cargo.nix
''
