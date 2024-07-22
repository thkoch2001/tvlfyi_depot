{ depot, lib, ... }:

depot.tvix.crates.workspaceMembers.nix-compat-derive.build.override {
  runTests = true;
}