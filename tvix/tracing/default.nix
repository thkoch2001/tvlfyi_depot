{ depot, lib, ... }:

(depot.tvix.crates.workspaceMembers.tvix-tracing.build.override {
  runTests = true;
}).overrideAttrs (old: rec {
  meta.ci.targets = lib.filter (x: lib.hasPrefix "with-features" x || x == "no-features") (lib.attrNames passthru);
  passthru = depot.tvix.utils.mkFeaturePowerset {
    inherit (old) crateName;
    features = [ "otlp" "tracy" "tonic" "reqwest" "axum" ];
  };
})
