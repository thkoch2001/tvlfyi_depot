{ depot, ... }:

depot.tvix.crates.workspaceMembers.nix-serialize-derive-tests.build.override {
  runTests = true;
}
