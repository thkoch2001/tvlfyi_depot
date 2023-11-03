{ depot, ... }:

(depot.tvix.crates.workspaceMembers.glue.build.override {
  runTests = true;
})
