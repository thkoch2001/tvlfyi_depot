{ depot, lib, ... }:

depot.tvix.crates.workspaceMembers.nix-serialize.build.override {
  features = [ "test" "derive" ];
  runTests = true;
}
