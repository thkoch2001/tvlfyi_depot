{ pkgs, depot, ... }:

# Run crate2nix generate in the current working directory, then
# format the generated file with depotfmt.
pkgs.writeShellScriptBin "crate2nix-generate" ''
  ${pkgs.crate2nix}/bin/crate2nix generate --all-features

  # Workaround for https://github.com/numtide/treefmt/issues/327
  if [[ "$HOME" == "/homeless-shelter" ]]; then
    ${depot.tools.depotfmt.passthru.check} Cargo.nix
  else
    ${depot.tools.depotfmt}/bin/depotfmt Cargo.nix
  fi
''
