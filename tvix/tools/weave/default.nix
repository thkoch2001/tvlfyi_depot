{ pkgs, depot, ... }:

(pkgs.callPackage ./Cargo.nix {
  defaultCrateOverrides = (depot.tvix.utils.defaultCrateOverridesForPkgs pkgs) // {
    weave = prev: {
      src = depot.tvix.utils.filterRustCrateSrc { root = prev.src.origSrc; };
    };
  };
}).rootCrate.build.overrideAttrs {
  meta.ci.extraSteps.crate2nix-check = depot.tvix.utils.mkCrate2nixCheck ./Cargo.nix;
}
