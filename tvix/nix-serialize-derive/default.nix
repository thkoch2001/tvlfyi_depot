{ depot, lib, ... }:

depot.tvix.crates.workspaceMembers.nix-serialize-derive.build.override {
  runTests = true;
}
