{ depot, lib, ... }:

(
  depot.nix.dependency-analyzer.knownDependencyGraph
    "depot"
    depot.ci.targets
).overrideAttrs (old: {
  meta = lib.recursiveUpdate (old.meta or { }) {
    ci.skip = true;
  };
})
