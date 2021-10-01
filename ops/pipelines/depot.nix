# This file configures the primary build pipeline used for the
# top-level list of depot targets.
{ depot, pkgs, ... }:

let
  # Protobuf check step which validates that changes to .proto files
  # between revisions don't cause backwards-incompatible or otherwise
  # flawed changes.
  protoCheck = {
    command = "${depot.nix.bufCheck}/bin/ci-buf-check";
    label = ":water_buffalo:";
  };

  # Formatting check which validates that all supported auto-formatted
  # files are formatted correctly. See //tools/depotfmt for details.
  depotfmtCheck = {
    command = "${depot.tools.depotfmt.check}";
    label = ":evergreen_tree: (tools/depotfmt)";
  };
  pipeline = depot.nix.buildkite.mkPipeline {
    headBranch = "refs/heads/canon";
    drvTargets = depot.ci.targets;
    skipIfBuilt = true;
    additionalSteps = [ depotfmtCheck protoCheck ];
  };

  drvmap = depot.nix.buildkite.mkDrvmap depot.ci.targets;
in pkgs.runCommandNoCC "depot-pipeline" ''
  mkdir $out
  cp -r ${pipeline}/* $out
  cp drvmap ${out}/drvmap.json
''
