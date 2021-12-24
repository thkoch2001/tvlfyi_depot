{ depot, pkgs, ... }:

(import ./Cargo.nix { inherit pkgs; }).rootCrate.build.override {
  runTests = true;
}
