{ depot, pkgs, lib, ... }:

((depot.tvix.crates.workspaceMembers.tvix-castore.build.override {
  runTests = true;
  testPreRun = ''
    export SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt;
  '';
}).overrideAttrs (oldAttrs: {
  meta.ci.targets = [ "integration-tests" ];
  passthru.integration-tests = depot.tvix.crates.workspaceMembers.${oldAttrs.crateName}.build.override (old: {
    runTests = true;
    testPreRun = ''
      export SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt;
      export PATH="$PATH:${lib.makeBinPath [ pkgs.cbtemulator pkgs.google-cloud-bigtable-tool ]}"
    '';
    features = old.features ++ [ "integration" ];
  });
})).overrideAttrs
  (oldAttrs: (depot.tvix.utils.mkFeaturePowerset {
    inherit oldAttrs;
    features = [ "cloud" "fuse" "tonic-reflection" ]
      # virtiofs feature currently fails to build on Darwin
      ++ lib.optional pkgs.stdenv.isLinux "virtiofs";
    override.testPreRun = ''
      export SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt
    '';
  }))
