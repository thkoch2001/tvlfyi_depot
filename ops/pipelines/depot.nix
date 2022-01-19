# This file configures the primary build pipeline used for the
# top-level list of depot targets.
{ depot, pkgs, externalArgs, ... }:

let
  # Protobuf check step which validates that changes to .proto files
  # between revisions don't cause backwards-incompatible or otherwise
  # flawed changes.
  protoCheck = {
    command = "${depot.nix.bufCheck}/bin/ci-buf-check";
    label = ":water_buffalo:";
  };

  pipeline = depot.nix.buildkite.mkPipeline {
    headBranch = "refs/heads/canon";
    drvTargets = depot.ci.targets;
    additionalSteps = [ protoCheck ];

    parentTargetMap = if (externalArgs ? parentTargetMap)
      then builtins.fromJSON (builtins.readFile externalArgs.parentTargetMap)
      else {};
  };

  drvmap = depot.nix.buildkite.mkDrvmap depot.ci.targets;
in pkgs.runCommandNoCC "depot-pipeline" {} ''
  mkdir $out
  cp -r ${pipeline}/* $out
  cp ${drvmap} $out/drvmap.json
''
