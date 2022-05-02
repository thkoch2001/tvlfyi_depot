# Shell derivation to invoke //nix/lazy-deps with the dependencies
# that should be lazily made available in depot.
{ pkgs, depot, ... }:

let
  deps = depot.nix.lazy-deps {
    age-keygen.attr = "third_party.nixpkgs.age";
    age.attr = "third_party.nixpkgs.age";
    depotfmt.attr = "tools.depotfmt";
    gerrit-update.attr = "tools.gerrit-update";
    gerrit.attr = "tools.gerrit-cli";
    hash-password.attr = "tools.hash-password";
    mg.attr = "tools.magrathea";
    nint.attr = "nix.nint";
    niv.attr = "third_party.nixpkgs.niv";
    rebuild-system.attr = "ops.nixos.rebuildSystem";
    rink.attr = "third_party.nixpkgs.rink";

    tf-glesys = {
      attr = "ops.glesys.terraform";
      cmd = "terraform";
    };

    tf-keycloak = {
      attr = "ops.keycloak.terraform";
      cmd = "terraform";
    };
  };
in
pkgs.mkShell {
  buildInputs = [
    deps
  ];
}
