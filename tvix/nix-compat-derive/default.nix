{ depot, lib, ... }:

(depot.tvix.crates.workspaceMembers.nix-compat-derive.build.override {
  runTests = true;
}).overrideAttrs (old: rec {
  meta.ci.targets = lib.filter (x: lib.hasPrefix "with-features" x || x == "no-features") (lib.attrNames passthru);
  passthru = depot.tvix.utils.mkFeaturePowerset {
    inherit (old) crateName;
    features = [];
  };
})
