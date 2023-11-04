{ depot, pkgs, ... }:

depot.tvix.crates.workspaceMembers.turbofetch.build.override {
  runTests = true;
}
