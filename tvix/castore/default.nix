{ depot, pkgs, lib, ... }:

(depot.tvix.crates.workspaceMembers.tvix-castore.build.override {
  runTests = true;
  testPreRun = ''
    export SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt;
  '';
}).overrideAttrs {
  meta.ci.targets = [ "integration-tests" ];
  passthru.integration-tests = pkgs.symlinkJoin {
    name = "tvix-castore-integration-tests";
    postBuild = "rm -rf $out/*"; # We want to clean up $out
    paths = (map
      (featuresPowerset: depot.tvix.crates.workspaceMembers.tvix-castore.build.override ({
        runTests = true;
        testPreRun = ''
          export SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt
          export PATH="$PATH:${lib.makeBinPath [ pkgs.cbtemulator pkgs.google-cloud-bigtable-tool ]}"
        '';
        features = [ "integration" ] ++ featuresPowerset;
      }))
      (depot.tvix.utils.powerset ([ "cloud" "fuse" "tonic-reflection" ]
        # virtiofs feature currently fails to build on Darwin
        ++ lib.optional pkgs.stdenv.isLinux "virtiofs")
      ));
  };
}
