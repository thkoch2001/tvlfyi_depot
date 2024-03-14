{ depot, pkgs, ... }:

depot.tvix.crates.workspaceMembers.tvix-build.build.override { runTests = true; }
