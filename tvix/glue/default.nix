{ depot, pkgs, ... }:

(depot.tvix.crates.workspaceMembers.tvix-glue.build.override {
  runTests = true;
  testPreRun = ''
    export SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt;
  '';
}).overrideAttrs (oldAttrs: depot.tvix.utils.mkFeaturePowerset {
  inherit oldAttrs;
  features = [ "nix_tests" ];
  override.testPreRun = ''
    export SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt;
  '';
})
