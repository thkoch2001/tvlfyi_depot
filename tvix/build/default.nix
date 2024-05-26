{ depot, pkgs, ... }:

(depot.tvix.crates.workspaceMembers.tvix-build.build.override {
  runTests = true;
}).overrideAttrs {
  meta.ci.targets = [ "integration-tests" ];
  passthru.integration-tests = pkgs.symlinkJoin {
    name = "tvix-build-integration-tests";
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

