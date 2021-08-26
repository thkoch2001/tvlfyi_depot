# This file sets up the top-level package set by traversing the package tree
# (see //nix/readTree for details) and constructing a matching attribute set
# tree.

{ nixpkgsBisectPath ? null, ... }@args:

let
  inherit (builtins)
    attrValues
    concatMap
    elem
    elemAt
    filter
    ;

  # This definition of fix is identical to <nixpkgs>.lib.fix, but the global
  # package set is not available here.
  fix = f: let x = f x; in x;

  # readTree argument filter to disallow certain usage patterns of
  # targets within depot.
  depotArgsFilter = args: parts:
    if (elemAt parts 0) == "users" || elem parts [
      # whitby is temporarily allowed to access //users because it
      # hosts tazjin's website until camden (or some replacement
      # machine) is back online.
      #
      # TODO(tazjin): Remove exception after website is moved.
      [ "ops" "machines" "whitby" ]
    ]
    then args
    else args // {
      depot = args.depot // {
        users = throw ''
          Access to items from the //users folder is not permitted from
          other depot paths. Code under //users is not considered stable
          or dependable in the wider depot context.

          If a project under //users is required by something else,
          please move it to a different depot path.

          At location: [ ${toString parts} ]
        '';
      };
    };

    readTree' = import ./nix/readTree {
      argsFilter = depotArgsFilter;
    };

  # To determine build targets, we walk through the depot tree and
  # fetch attributes that were imported by readTree and are buildable.
  #
  # Any build target that contains `meta.ci = false` will be skipped.

  # Is this tree node eligible for build inclusion?
  eligible = node: (node ? outPath) && (node.meta.ci or true);

  # Walk the tree starting with 'node', recursively extending the list
  # of build targets with anything that looks buildable.
  #
  # Any tree node can specify logical targets by exporting a
  # 'meta.targets' attribute containing a list of keys in itself. This
  # enables target specifications that do not exist on disk directly.
  gather = node:
    if node ? __readTree then
      # Include the node itself if it is eligible.
      (if eligible node then [ node ] else [])
      # Include eligible children of the node
      ++ concatMap gather (attrValues node)
      # Include specified sub-targets of the node
      ++ filter eligible (map
           (k: (node."${k}" or {}) // {
             # Keep the same tree location, but explicitly mark this
             # node as a subtarget.
             __readTree = node.__readTree;
             __subtarget = k;
           })
           (node.meta.targets or []))
    else [];

in fix(self: (readTree' {
  depot = self;

  # Pass third_party as 'pkgs' (for compatibility with external
  # imports for certain subdirectories)
  pkgs = self.third_party.nixpkgs;

  # Expose lib attribute to packages.
  lib = self.third_party.nixpkgs.lib;

  # Pass arguments passed to the entire depot through, for packages
  # that would like to add functionality based on this.
  #
  # Note that it is intended for exceptional circumstance, such as
  # debugging by bisecting nixpkgs.
  externalArgs = args;
} ./.) // {
  # Make the path to the depot available for things that might need it
  # (e.g. NixOS module inclusions)
  path = ./.;

  # List of all buildable targets, for CI purposes.
  #
  # Note: To prevent infinite recursion, this *must* be a nested
  # attribute set (which does not have a __readTree attribute).
  ci.targets = gather (self // {
    # remove the pipelines themselves from the set over which to
    # generate pipelines because that also leads to infinite
    # recursion.
    ops = self.ops // { pipelines = null; };

    # remove nixpkgs from the set, for obvious reasons.
    third_party = self.third_party // { nixpkgs = null; };
  });

  # Derivation that gcroots all depot targets.
  ci.gcroot = self.third_party.nixpkgs.symlinkJoin {
    name = "depot-gcroot";
    paths = self.ci.targets;
  };
})
