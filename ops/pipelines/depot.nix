# This file configures the primary build pipeline used for the
# top-level list of depot targets.
{ depot, ... }:

let
  # Protobuf check step which validates that changes to .proto files
  # between revisions don't cause backwards-incompatible or otherwise
  # flawed changes.
  protoCheck = {
    command = "${depot.nix.bufCheck}/bin/ci-buf-check";
    label = ":water_buffalo:";
  };
in depot.nix.buildkite.mkPipeline {
  headBranch = "refs/heads/canon";
  drvTargets = depot.ci.targets;
  skipIfBuilt = true;
  additionalSteps = [ protoCheck ];
}
