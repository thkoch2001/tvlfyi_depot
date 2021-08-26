# This file configures the primary build pipeline used for the
# top-level list of depot targets.
#
# It outputs a "YAML" (actually JSON) file which is evaluated and
# submitted to Buildkite at the start of each build. This means we can
# dynamically configure the pipeline execution here.
{ depot, lib, pkgs, ... }:

let
  inherit (builtins) concatStringsSep foldl' map toJSON;
  inherit (pkgs) symlinkJoin writeText;

  # Create an expression that builds the target at the specified
  # location.
  mkBuildExpr = target:
    let
      descend = expr: attr: "builtins.getAttr \"${attr}\" (${expr})";
      targetExpr = foldl' descend "import ./. {}" target.__readTree;
      subtargetExpr = descend targetExpr target.__subtarget;
    in if target ? __subtarget then subtargetExpr else targetExpr;

  # Create a pipeline label from the targets tree location.
  mkLabel = target:
    let label = concatStringsSep "/" target.__readTree;
    in if target ? __subtarget
      then "${label}:${target.__subtarget}"
      else label;

  # Create a pipeline step from a single target.
  #
  # If the build fails, Buildkite metadata is updated to mark the
  # pipeline as failed. Buildkite has a concept of a failed pipeline
  # regardless, but this data is not accessible.
  mkStep = target: {
    command = let
      drvPathNoCtx = builtins.unsafeDiscardStringContext target.drvPath;
      # referencing a .drv path by default creates a dependency via a
      # string context of { allOutputs = true; }. Which would cause
      # Nix to build all targets while constructing the pipeline.
      # To work around this we change the context to only require
      # { path = true; } which makes sure the resulting depot.yaml
      # references the .drv paths and that they are alive as long
      # as the depot.yaml is alive.
      newCtx = {
        ${drvPathNoCtx} = { path = true; };
      };
      drvPath = builtins.appendContext drvPathNoCtx newCtx;
    in lib.concatStringsSep " || " [
      "nix-store --realise '${drvPath}'"
      "(buildkite-agent meta-data set 'failure' '1'; exit 1)"
    ];
    label = ":nix: ${mkLabel target}";

    # Skip build steps if their out path has already been built.
    skip = let
      shouldSkip = with builtins;
        # Only skip in real Buildkite builds
        (getEnv "BUILDKITE_BUILD_ID" != "") &&
        # Always build everything for the canon branch.
        (getEnv "BUILDKITE_BRANCH" != "refs/heads/canon") &&
        # Discard string context to avoid realising the store path during
        # pipeline construction.
        (pathExists (unsafeDiscardStringContext target.outPath));
      in if shouldSkip then "Target was already built." else false;
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
        key = ":duck:";
      })

      # After duck, on success, create a gcroot if the build branch is
      # canon.
      #
      # We care that this anchors *most* of the depot, in practice
      # it's unimportant if there is a build race and we get +-1 of
      # the targets.
      #
      # Unfortunately this requires a third evaluation of the graph,
      # but since it happens after :duck: it should not affect the
      # timing of status reporting back to Gerrit.
      ({
        command = "nix-instantiate -A ci.gcroot --add-root /nix/var/nix/gcroots/depot/canon";
        label = ":anchor:";
        "if" = ''build.branch == "refs/heads/canon"'';
        depends_on = [{
          step = ":duck:";
          allow_failure = false;
        }];
      })
    ];
in (writeText "depot.yaml" (toJSON pipeline))
