{ depot, ... }:

(depot.tvix.crates.workspaceMembers.nix-compat.build.override {
  runTests = true;
}).overrideAttrs (oldAttrs: depot.tvix.utils.mkFeaturePowerset {
  inherit oldAttrs;
  features = [ "async" "wire" ];
})
