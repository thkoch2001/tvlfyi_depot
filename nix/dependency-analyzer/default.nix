{ lib, depot, pkgs, ... }:

let
  inherit (builtins) unsafeDiscardStringContext appendContext;

  #
  # Utilities
  #

  # Manipulate string context of the given string so that it only carries a
  # `path` reference to itself (so it needs to be a string representation of
  # a store path).
  #
  # This is intended for use on the `drvPath` attribute of derivations which by
  # default carries a reference to the correspoindng outputs. If we only want to
  # read from the `drvPath`, having only a `path` reference makes sure we don't
  # need to realise the derivation first.
  #
  # Type: str -> str
  pathContextDrvPath = drvPath:
    let
      drvPath' = unsafeDiscardStringContext drvPath;
    in
    appendContext drvPath' { ${drvPath'} = { path = true; }; };

  # Find all quoted references to a derivation path in the specified drv file.
  # Should correspond to the list of input derivations, but is obviously a big
  # HACK as we just grep for store paths that look right. This should eventually
  # be solved properly by parsing the drv file.
  #
  # Type: str -> [str]
  directDrvDeps = drvPath: builtins.concatLists (
    builtins.filter builtins.isList (
      builtins.split
        "\"(${lib.escapeRegex builtins.storeDir}/[[:alnum:]+._?=-]+.drv)\""
        (builtins.readFile drvPath)
    )
  );

  # Maps a list of derivation to the list of corresponding `drvPath`s.
  #
  # Type: [drv] -> [str]
  drvsToPaths = drvs:
    builtins.map (drv: pathContextDrvPath drv.drvPath) drvs;

  #
  # Calculate map of direct derivation dependencies
  #

  # Create the dependency map entry for a given `drvPath` which mainly includes
  # a list of other `drvPath`s it depends on. Additionally we store whether the
  # derivation is `known`, i.e. part of the initial list of derivations we start
  # generating the map from
  #
  # Type: bool -> string -> set
  drvEntry = known: drvPath:
    let
      # key may not refer to a store path, …
      key = unsafeDiscardStringContext drvPath;
      # but we must read from the .drv file.
      path = pathContextDrvPath drvPath;
    in
    {
      inherit key;
      # trick so we can call listToAttrs directly on the result of genericClosure
      name = key;
      value = {
        deps = directDrvDeps path;
        inherit known;
      };
    };

  # Create an attribute set that maps every derivation in the combined
  # dependency closure of the list of input derivation paths to every of their
  # direct dependencies. Additionally every entry will have set their `known`
  # attribute to `true` if it is in the list of input derivation paths.
  #
  # Type: [str] -> set
  plainDrvDepMap = drvPaths:
    builtins.listToAttrs (
      builtins.genericClosure {
        startSet = builtins.map (drvEntry true) drvPaths;
        operator = { value, ... }: builtins.map (drvEntry false) value.deps;
      }
    );

  #
  # Calculate closest known dependencies in the dependency map
  #

  inherit (depot.nix.stateMonad)
    after
    bind
    for_
    get
    getAttr
    run
    setAttr
    pure
    ;

  # This is an action in stateMonad which expects the (initial) state to have
  # been produced by `plainDrvDepMap`. Given a `drvPath`, it calculates a
  # `knownDeps` list which holds the `drvPath`s of the closest derivation marked
  # as `known` along every edge. This list is inserted into the dependency map
  # for `drvPath` and every other derivation in its dependecy closure (unless
  # the information was already present). This means that the known dependency
  # information for a derivation never has to be recalculated, as long as they
  # are part of the same stateful computation.
  #
  # The upshot is that after calling `insertKnownDeps drvPath`,
  # `fmap (builtins.getAttr "knownDeps") (getAttr drvPath)` will always succeed.
  #
  # Type: str -> stateMonad drvDepMap null
  insertKnownDeps = drvPathWithContext:
    let
      # We no longer need to read from the store, so context is irrelevant, but
      # we need to check for attr names which requires the absence of context.
      drvPath = unsafeDiscardStringContext drvPathWithContext;
    in
    bind get (initDepMap:
      # Get the dependency map's state before we've done anything to obtain the
      # entry we'll be manipulating later as well as its dependencies.
      let
        entryPoint = initDepMap.${drvPath};

        # We don't need to recurse if our direct dependencies either have their
        # knownDeps list already populated or are known dependencies themselves.
        depsPrecalculated =
          builtins.partition
            (dep:
              initDepMap.${dep}.known
              || initDepMap.${dep} ? knownDeps
            )
            entryPoint.deps;

        # If a direct dependency is known, it goes right to our known dependency
        # list. If it is unknown, we can copy its knownDeps list into our own.
        initiallyKnownDeps =
          builtins.concatLists (
            builtins.map
              (dep:
                if initDepMap.${dep}.known
                then [ dep ]
                else initDepMap.${dep}.knownDeps
              )
              depsPrecalculated.right
          );
      in

      # If the information was already calculated before, we can exit right away
      if entryPoint ? knownDeps
      then pure null
      else
        after
          # For all unknown direct dependencies which don't have a `knownDeps`
          # list, we call ourselves recursively to populate it. Since this is
          # done sequentially in the state monad, we avoid recalculating the
          # list for the same derivation multiple times.
          (for_
            depsPrecalculated.wrong
            insertKnownDeps)
          # After this we can obtain the updated dependency map which will have
          # a `knownDeps` list for all our direct dependencies and update the
          # entry for the input `drvPath`.
          (bind
            get
            (populatedDepMap:
              (setAttr drvPath (entryPoint // {
                knownDeps =
                  lib.unique (
                    initiallyKnownDeps
                      ++ builtins.concatLists (
                      builtins.map
                        (dep: populatedDepMap.${dep}.knownDeps)
                        depsPrecalculated.wrong
                    )
                  );
              }))))
    );

  # This function puts it all together and is exposed via `__functor`.
  #
  # For a list of `drvPath`s, calculate an attribute set which maps every
  # `drvPath` to a set of the following form:
  #
  #     {
  #       known = true /* if it is in the list of input derivation paths */;
  #       deps = [
  #         /* list of derivation paths it depends on directly */
  #       ];
  #       knownDeps = [
  #         /* list of the closest derivation paths marked as known this
  #            derivation depends on.
  #         */
  #       ];
  #     }
  knownDrvDepMap = knownDrvPaths:
    run
      (plainDrvDepMap knownDrvPaths)
      (after
        (for_
          knownDrvPaths
          insertKnownDeps)
        get);

  #
  # Other things based on knownDrvDepMap
  #

  # Create a SVG visualizing `knownDrvDepMap`. Nodes are identified by derivation
  # name, so multiple entries can be collapsed if they have the same name.
  #
  # Type: [drv] -> drv
  knownDependencyGraph = name: drvs:
    let
      justName = drvPath:
        builtins.substring
          (builtins.stringLength builtins.storeDir + 1 + 32 + 1)
          (builtins.stringLength drvPath)
          (unsafeDiscardStringContext drvPath);

      gv = pkgs.writeText "${name}-dependency-analysis.gv" ''
        digraph depot {
        ${
          (lib.concatStringsSep "\n"
          (lib.mapAttrsToList (name: value:
            if !value.known then ""
            else lib.concatMapStringsSep "\n"
              (knownDep: "  \"${justName name}\" -> \"${justName knownDep}\"")
              value.knownDeps
          )
          (depot.nix.dependency-analyzer (
            drvsToPaths drvs
          ))))
        }
        }
      '';
    in

    pkgs.runCommand "${name}-dependency-analysis.svg"
      {
        nativeBuildInputs = [
          pkgs.buildPackages.graphviz
        ];
      }
      "dot -Tsvg < ${gv} > $out";
in

{
  __functor = _: knownDrvDepMap;

  inherit knownDependencyGraph plainDrvDepMap drvsToPaths;

  # mg build //nix/dependency-analyzer:example; intentionally ignored by CI
  example =
    knownDependencyGraph
      "depot"
      depot.ci.targets
    # (builtins.filter lib.isDerivation (builtins.attrValues depot.third_party.lisp))
  ;
}
