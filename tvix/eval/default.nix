# TODO: find a way to build the benchmarks via crate2nix
{ depot, pkgs, ... }:

(depot.tvix.crates.workspaceMembers.tvix-eval.build.override {
  runTests = true;

  # Make C++ Nix available, to compare eval results against.
  testInputs = [ pkgs.nix ];
}).overrideAttrs {
  meta.ci.targets = [ "feature-permutations" ];
  passthru.feature-permutations = pkgs.symlinkJoin {
    name = "tvix-eval-feature-permutations";
    postBuild = "rm -rf $out/*"; # We want to clean up $out
    paths = (map
      (featuresPowerset: depot.tvix.crates.workspaceMembers.tvix-eval.build.override ({
        runTests = true;
        testInputs = [ pkgs.nix ];
        features = featuresPowerset;
      }))
      (depot.tvix.utils.powerset [ "impure" "arbitrary" "nix_tests" ])
    );
  };
}
