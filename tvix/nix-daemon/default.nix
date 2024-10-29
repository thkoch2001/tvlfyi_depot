{ depot, ... }:

depot.tvix.crates.workspaceMembers.nix-daemon.build.override {
  runTests = true;
}