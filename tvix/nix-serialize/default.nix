{ depot, lib, ... }:

depot.tvix.crates.workspaceMembers.nix-serialize.build.override {
  runTests = true;
}
