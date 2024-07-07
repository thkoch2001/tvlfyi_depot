{ depot, pkgs, lib, ... }:

(depot.tvix.crates.workspaceMembers.tvix-glue.build.override {
  runTests = true;
  testPreRun = ''
    export SSL_CERT_FILE=/dev/null
  '';
}).overrideAttrs (old: rec {
  meta.ci.targets = lib.filter (x: lib.hasPrefix "with-features" x || x == "no-features") (lib.attrNames passthru);
  passthru = depot.tvix.utils.mkFeaturePowerset {
    inherit (old) crateName;
    features = [ "nix_tests" ];
    override.testPreRun = ''
      export SSL_CERT_FILE=/dev/null
    '';
  };
})
