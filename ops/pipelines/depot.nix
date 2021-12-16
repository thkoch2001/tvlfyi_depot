# This file configures the primary build pipeline used for the
# top-level list of depot targets.
#
# It outputs a "YAML" (actually JSON) file which is evaluated and
# submitted to Buildkite at the start of each build. This means we can
# dynamically configure the pipeline execution here.
{ depot, lib, pkgs, ... }:

let
  inherit (builtins)
    attrValues
    concatStringsSep
    foldl'
    length
    map
    mapAttrs
    toJSON
    concatMap;

  inherit (pkgs) runCommandNoCC symlinkJoin writeText;

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

  # Should this target be skipped?
  shouldSkip = target: with builtins;
    # Only skip in real Buildkite builds
    (getEnv "BUILDKITE_BUILD_ID" != "") &&
    # Always build everything for the canon branch.
    (getEnv "BUILDKITE_BRANCH" != "refs/heads/canon") &&
    # Discard string context to avoid realising the store path during
    # pipeline construction.
    (pathExists (unsafeDiscardStringContext target.outPath));

  sanitizeBuildkiteKey = builtins.replaceStrings ["/" "." ":"] ["-" "-" "--"];

  # Create a pipeline step from a single target.
  mkTargetStep = target: let
    drvPath = builtins.unsafeDiscardStringContext target.drvPath;
  in {
    key = sanitizeBuildkiteKey (mkLabel target);
    command = lib.concatStringsSep " " [
      # First try to realise the drvPath of the target so we don't evaluate twice.
      # Nix has no concept of depending on a derivation file without depending on
      # at least one of its `outPath`s, so we need to discard the string context
      # if we don't want to build everything during pipeline construction.
      "nix-store --realise '${drvPath}'"
      # Since we don't gcroot the derivation files, they may be deleted by the
      # garbage collector. In that case we can reevaluate and build the attribute
      # using nix-build.
      "|| (test ! -f '${drvPath}' && nix-build -E '${mkBuildExpr target}' --show-trace)"
    ];
    label = ":nix: ${mkLabel target}";

    # Skip build steps if their out path has already been built.
    skip = if shouldSkip target
           then "Target was already built."
           else false;

    # Add a "fake" dependency on the initial static pipeline step. When
    # uploading a pipeline dynamically, an implicit dependency on the uploading
    # step is added to all newly created build steps. Since we are uploading in
    # batches this stops the jobs in the first batch from running before all
    # batches have been uploaded.
    #
    # By setting an explicit dependency on a step that has always completed at
    # this point, we override that behaviour and allow the steps to start
    # running already.
    depends_on = ":init:";
  };

  mkExtraStep = dependsOn: { label, command }: {
    inherit label command;
    depends_on = dependsOn;
  };

  mkSteps = target: let
    targetStep = mkTargetStep target;
  in [targetStep]
     ++ (if (target.skip or false) != false
         then []
         else (map (mkExtraStep target.key) (target.extraSteps or [])));

  # Protobuf check step which validates that changes to .proto files
  # between revisions don't cause backwards-incompatible or otherwise
  # flawed changes.
  protoCheck = {
    command = "${depot.nix.bufCheck}/bin/ci-buf-check";
    label = ":water_buffalo:";
  };

  # All pipeline steps before batching them into smaller chunks.
  allSteps =
    # Create build steps for each CI target
    (concatMap mkSteps depot.ci.targets)

    ++ [
      # Simultaneously run protobuf checks
      protoCheck
    ];

  # Helper function to inelegantly divide a list into chunks of at
  # most n elements.
  #
  # This works by assigning each element a chunk ID based on its
  # index, and then grouping all elements by their chunk ID.
  chunksOf = n: list: let
    chunkId = idx: toString (idx / n + 1);
    assigned = lib.imap1 (idx: value: { inherit value ; chunk = chunkId idx; }) list;
    unchunk = mapAttrs (_: elements: map (e: e.value) elements);
  in unchunk (lib.groupBy (e: e.chunk) assigned);

  # Define a build pipeline chunk as a JSON file, using the pipeline
  # format documented on
  # https://buildkite.com/docs/pipelines/defining-steps.
  makePipelineChunk = chunkId: chunk: rec {
    filename = "chunk-${chunkId}.json";
    path = writeText filename (toJSON {
      steps = chunk;
    });
  };

  pipelineChunks = attrValues (mapAttrs makePipelineChunk (chunksOf 256 allSteps));

in runCommandNoCC "depot-pipeline" {} ''
  mkdir $out
  echo "Generated ${toString (length pipelineChunks)} pipeline chunks"
  ${
    lib.concatMapStringsSep "\n"
      (chunk: "cp ${chunk.path} $out/${chunk.filename}") pipelineChunks
  }
''
