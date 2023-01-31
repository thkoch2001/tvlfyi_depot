{ depot, ... }:

depot.tvix.crates.workspaceMembers.tvix-nix-compat.build.override {
  runTests = true;
}
