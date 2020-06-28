# This file configures the primary build pipeline used for the
# top-level list of depot targets.
#
# It outputs a "YAML" (actually JSON) file which is evaluated and
# submitted to Buildkite at the start of each build. This means we can
# dynamically configure the pipeline execution here.
{ depot, lib, pkgs, ... }:

let
  inherit (builtins) getEnv toJSON;
  inherit (pkgs) writeText;

  # This Nix expression is evaluated when a Buildkite pipeline is
  # launched and, as you can see below, needs to do some impure things
  # to decide the shape of the pipeline.
  orString = s: d: if s == "" then d else s;
  pipelineBranch = orString (getEnv "BUILDKITE_BRANCH") "canon";

  gerritUpdate = lib.optionals (lib.hasPrefix "refs/changes/" pipelineBranch) [
  ];

  # This defines the build pipeline, using the pipeline format
  # documented on https://buildkite.com/docs/pipelines/defining-steps
  pipeline.steps = [
    {
      command = "nix-build -A ciBuilds.__allTargets";
      label = "allTargets";
    }
  ] ++ gerritUpdate;
in writeText "depot.yaml" (toJSON pipeline)
