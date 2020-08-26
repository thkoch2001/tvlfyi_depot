# This file configures the primary build pipeline used for the
# top-level list of depot targets.
#
# It outputs a "YAML" (actually JSON) file which is evaluated and
# submitted to Buildkite at the start of each build. This means we can
# dynamically configure the pipeline execution here.
{ depot, pkgs, ... }:

let
  inherit (builtins) toJSON;
  inherit (pkgs) writeText;

  # This defines the build pipeline, using the pipeline format
  # documented on https://buildkite.com/docs/pipelines/defining-steps
  pipeline.steps = [
    {
      command = "nix-build -A ci.targets --show-trace";
      label = ":duck:";
    }
    {
      command = "${depot.nix.bufCheck}/bin/ci-buf-check";
      label = ":water_buffalo:";
    }
  ];
in writeText "depot.yaml" (toJSON pipeline)
