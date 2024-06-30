{ depot, ... }:

(depot.tvix.crates.workspaceMembers.tvix-build.build.override {
  runTests = true;
}).overrideAttrs (oldAttrs:
depot.tvix.utils.mkFeaturePowerset {
  inherit oldAttrs;
  features = [ "tonic-reflection" ];
})
