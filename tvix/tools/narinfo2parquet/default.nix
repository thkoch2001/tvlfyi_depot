{ pkgs, depot, ... }:

(pkgs.callPackage ./Cargo.nix {
  defaultCrateOverrides = (depot.tvix.utils.defaultCrateOverridesForPkgs pkgs) // {
    narinfo2parquet = prev: {
      src = depot.tvix.utils.filterRustCrateSrc { root = prev.src.origSrc; };
    };
  };
}).rootCrate.build.overrideAttrs {
  meta.ci.extraSteps.crate2nix = depot.utils.mkCrate2nixCheck ./Cargo.nix;
}
