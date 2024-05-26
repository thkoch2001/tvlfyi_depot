{ depot, pkgs, ... }:

(depot.tvix.crates.workspaceMembers.tvix-glue.build.override {
  runTests = true;
  testPreRun = ''
    export SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt;
  '';
}).overrideAttrs {
  meta.ci.targets = [ "feature-permutations" ];
  passthru.feature-permutations = pkgs.symlinkJoin {
    name = "tvix-glue-feature-permutations";
    postBuild = "rm -rf $out/*"; # We want to clean up $out
    paths = (map
      (featuresPowerset: depot.tvix.crates.workspaceMembers.tvix-glue.build.override ({
        runTests = true;
        testPreRun = ''
          export SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt;
        '';
        features = featuresPowerset;
      }))
      (depot.tvix.utils.powerset [ "nix_tests" ])
    );
  };
}

