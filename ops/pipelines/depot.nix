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
  mkStep = target: {
    command = let
      drvPath = builtins.unsafeDiscardStringContext target.drvPath;
    in lib.concatStringsSep " " [
      # First try to realise the drvPath of the target so we don't evaluate twice.
      # Nix has no concept of depending on a derivation file without depending on
      # at least one of its `outPath`s, so we need to discard the string context
      # if we don't want to build everything during pipeline construction.
      "nix-store --realise '${drvPath}'"
      # However, Nix doesn't track references of store paths to derivations, so
      # there's no guarantee that the derivation file is not garbage collected.
      # To handle this case we fall back to an ordinary build if the derivation
      # file is missing.
      "|| (test ! -f '${drvPath}' && nix-build -E '${mkBuildExpr target}' --show-trace)"
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
    # Create build steps for each CI target
    (map mkStep depot.ci.targets)

    ++ [
      # Simultaneously run protobuf checks
      protoCheck

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
