{ pkgs, depot, ... }:

(pkgs.callPackage ./Cargo.nix {
  defaultCrateOverrides = (depot.tvix.utils.defaultCrateOverridesForPkgs pkgs) // {
    turbofetch = prev: {
      src = depot.tvix.utils.filterRustCrateSrc { root = prev.src.origSrc; };
    };
  };
}).rootCrate.build
