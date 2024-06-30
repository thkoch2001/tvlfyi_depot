{ depot, ... }:

(depot.tvix.crates.workspaceMembers.tvix-tracing.build.override {
  runTests = true;
}).overrideAttrs (oldAttrs: depot.tvix.utils.mkFeaturePowerset {
  inherit oldAttrs;
  features = [ "otlp" "tracy" ];
})
