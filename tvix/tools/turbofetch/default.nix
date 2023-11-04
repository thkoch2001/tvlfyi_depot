{ pkgs, ... }:

(pkgs.callPackage ./Cargo.nix {
  defaultCrateOverrides = pkgs.defaultCrateOverrides // {
    ring = prev: {
      # TODO(edef): implement CARGO_MANIFEST_LINKS in crate2nix
      CARGO_MANIFEST_LINKS = ''ring_core_${lib.replaceStrings ["."] ["_"] prev.version}'';
    };
  };
}).rootCrate.build.override {
  runTests = true;
}
