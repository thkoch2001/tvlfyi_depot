{ depot, pkgs, ... }:

(depot.tvix.crates.workspaceMembers.nar-bridge.build.override {
  runTests = true;
}).overrideAttrs (old: rec {
  passthru = (depot.tvix.utils.mkFeaturePowerset {
    inherit (old) crateName;
    features = [ "otlp" ];
  });
})
