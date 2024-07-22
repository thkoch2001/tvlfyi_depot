{ depot, ... }:

depot.tvix.crates.workspaceMembers.nix-compat-derive-tests.build.override {
  runTests = true;
}