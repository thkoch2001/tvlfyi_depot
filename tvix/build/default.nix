{ depot, pkgs, ... }:

(depot.tvix.crates.workspaceMembers.tvix-build.build.override {
  runTests = true;
}).overrideAttrs {
  meta.ci.targets = [ "feature-permutations" ];
  passthru.feature-permutations = pkgs.symlinkJoin {
    name = "tvix-build-feature-permutations";
    postBuild = "rm -rf $out/*"; # We want to clean up $out
    paths = (map
      (featuresPowerset: depot.tvix.crates.workspaceMembers.tvix-build.build.override ({
        runTests = true;
        features = featuresPowerset;
      }))
      (depot.tvix.utils.powerset [ "tonic-reflection" ])
    );
  };
}

