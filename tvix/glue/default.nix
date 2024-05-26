{ depot, pkgs, ... }:

(depot.tvix.crates.workspaceMembers.tvix-glue.build.override {
  runTests = true;
  testPreRun = ''
    export SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt;
  '';
}).overrideAttrs {
  meta.ci.targets = [ "integration-tests" ];
  passthru.integration-tests = pkgs.symlinkJoin {
    name = "tvix-glue-integration-tests";
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

