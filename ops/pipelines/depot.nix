# This file configures the primary build pipeline used for the
# top-level list of depot targets.
#
# It outputs a "YAML" (actually JSON) file which is evaluated and
# submitted to Buildkite at the start of each build. This means we can
# dynamically configure the pipeline execution here.
{ depot, pkgs, ... }:

let
  inherit (builtins) map toJSON;
  inherit (pkgs) writeText;

  # Create a pipeline step from a single target.
  mkStep = target: {
    command = "nix-build ${target.drvPath}";
    label = ":nix: ${target.__readTree}";
  };

  # This defines the build pipeline, using the pipeline format
  # documented on https://buildkite.com/docs/pipelines/defining-steps
  #
  # TODO(tazjin): We don't have a :duck:-step anymore, this kills the
  # joke.
  pipeline.steps = (map mkStep depot.ci.targets) ++ [{
      command = "${depot.nix.bufCheck}/bin/ci-buf-check";
      label = ":water_buffalo:";
    }];
in (writeText "depot.yaml" (toJSON pipeline))
