# TODO: find a way to build the benchmarks via crate2nix
{ depot, pkgs, lib, ... }:

(depot.tvix.crates.workspaceMembers.tvix-eval.build.override {
  runTests = true;

  # Make C++ Nix available, to compare eval results against.
  testInputs = [ pkgs.nix ];
}).overrideAttrs (old: rec {
  meta.ci.targets = lib.filter (x: lib.hasPrefix "with-features" x || x == "no-features") (lib.attrNames passthru);
  passthru = old.passthru // (depot.tvix.utils.mkFeaturePowerset {
    inherit (old) crateName;
    features = [ "nix_tests" ];
    override.testInputs = [ pkgs.nix ];
  });
})
