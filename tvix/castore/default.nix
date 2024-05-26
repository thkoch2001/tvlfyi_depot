{ depot, pkgs, lib, ... }:

(depot.tvix.crates.workspaceMembers.tvix-castore.build.override {
  runTests = true;
  testPreRun = ''
    export SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt;
  '';
}).overrideAttrs {
  meta.ci.targets = [ "integration-tests" "feature-permutations" ];
  passthru = {
    integration-tests = depot.tvix.crates.workspaceMembers.tvix-castore.build.override (old: {
      runTests = true;
      testPreRun = ''
        export SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt;
        export PATH="$PATH:${pkgs.lib.makeBinPath [ pkgs.cbtemulator pkgs.google-cloud-bigtable-tool ]}"
      '';
      # enable some optional features.
      features = old.features ++ [ "cloud" "integration" ];
    });
    feature-permutations = pkgs.symlinkJoin {
      name = "tvix-castore-feature-permutations";
      postBuild = "rm -rf $out/*"; # We want to clean up $out
      paths = (map
        (featuresPowerset: depot.tvix.crates.workspaceMembers.tvix-castore.build.override ({
          runTests = true;
          testPreRun = ''
            export SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt
          '';
          features = featuresPowerset;
        }))
        (depot.tvix.utils.powerset ([ "cloud" "fuse" "tonic-reflection" ]
          # virtiofs feature currently fails to build on Darwin
          ++ lib.optional pkgs.stdenv.isLinux "virtiofs")
        ));
    };
  };
}
