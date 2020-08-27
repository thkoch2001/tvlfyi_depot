# This file configures the primary build pipeline used for the
# top-level list of depot targets.
#
# It outputs a "YAML" (actually JSON) file which is evaluated and
# submitted to Buildkite at the start of each build. This means we can
# dynamically configure the pipeline execution here.
{ depot, lib, pkgs, ... }:

let
  inherit (builtins) map toJSON;
  inherit (lib) singleton;
  inherit (pkgs) writeText;

  # Create a pipeline step from a single target.
  #
  # If the build fails, Buildkite metadata is updated to mark the
  # pipeline as failed. Buildkite has a concept of a failed pipeline
  # regardless, but this data is not accessible.
  mkStep = target: {
    command = ''
      nix-build ${target.drvPath} || buildkite-agent meta-data set "failure" "1"
    '';
    label = ":nix: ${target.name}";
  };

  # Protobuf check step which validates that changes to .proto files
  # between revisions don't cause backwards-incompatible or otherwise
  # flawed changes.
  protoCheck = {
    command = "${depot.nix.bufCheck}/bin/ci-buf-check";
    label = ":water_buffalo:";
  };

  # This defines the build pipeline, using the pipeline format
  # documented on https://buildkite.com/docs/pipelines/defining-steps
  #
  # Pipeline steps need to stay in order.
  pipeline.steps =
    # Zero the failure status
    [
      {
        command = "buildkite-agent meta-data set 'failure' '0'";
        label = ":buildkite:";
      }
      { wait = null; }
    ]

    # Create build steps for each CI target
    ++ (map mkStep depot.ci.targets)

    ++ [
      # Simultaneously run protobuf checks
      protoCheck

      # Wait for all previous checks to complete
      ({
        wait = null;
        continue_on_failure = true;
      })

      # Wait for all steps to complete, then exit with success or
      # failure depending on whether any failure status was written.
      # This step must be :duck:! (yes, really!)
      ({
        command = "exit $(buildkite-agent meta-data get 'failure')";
        label = ":duck:";
      })
    ];
in (writeText "depot.yaml" (toJSON pipeline))
