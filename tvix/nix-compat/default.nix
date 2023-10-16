{ depot, ... }:

depot.tvix.crates.workspaceMembers.nix-compat.build.override {
  runTests = true;
  # make sure we also enable async here, so run the tests behind that feature flag.
  features = [ "default" "async" ];
}
