{ depot, pkgs, lib, ... }:

(depot.tvix.crates.workspaceMembers.tvix-castore.build.override {
  runTests = true;
  testPreRun = ''
    export SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt;
  '';
}).overrideAttrs (old: rec {
  meta.ci.targets = [ "integration-tests" ] ++ lib.filter (x: lib.hasPrefix "with-features" x || x == "no-features") (lib.attrNames passthru);
  passthru = (depot.tvix.utils.mkFeaturePowerset {
    inherit (old) crateName;
    features = ([ "cloud" "fuse" "tonic-reflection" ]
      # virtiofs feature currently fails to build on Darwin
      ++ lib.optional pkgs.stdenv.isLinux "virtiofs");
    override.testPreRun = ''
      export SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt
    '';
  }) // {
    integration-tests = depot.tvix.crates.workspaceMembers.${old.crateName}.build.override (old: {
      runTests = true;
      testPreRun = ''
        export SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt;
        export PATH="$PATH:${pkgs.lib.makeBinPath [ pkgs.cbtemulator pkgs.google-cloud-bigtable-tool ]}"
      '';
      # enable some optional features.
      features = old.features ++ [ "cloud" "integration" ];
    });
  };
})
