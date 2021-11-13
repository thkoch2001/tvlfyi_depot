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

  # Create a readTree filter disallowing access to the specified
  # top-level folder in other parts of the depot, except for specific
  # exceptions specified by their (full) paths.
  restrictFolder = { folder, exceptions ? [], reason }: parts: args:
    if (elemAt parts 0) == folder || elem parts exceptions
    then args
    else args // {
      depot = args.depot // {
        "${folder}" = throw ''
          Access to targets under //${folder} is not permitted from
          other depot paths. Specific exceptions are configured at the
          top-level.

          ${reason}
          At location: //${builtins.concatStringsSep "/" parts}
        '';
      };
    };

  # Disallow access to //users from other depot parts.
  usersFilter = restrictFolder {
    folder = "users";
    reason = ''
      Code under //users is not considered stable or dependable in the
      wider depot context. If a project under //users is required by
      something else, please move it to a different depot path.
    '';

    exceptions = [
      # whitby is allowed to access //users for several reasons:
      #
      # 1. User SSH keys are set in //users.
      # 2. Some personal websites or demo projects are served from it.
      [ "ops" "machines" "whitby" ]

      # Due to evaluation order this also affects these targets.
      # TODO(tazjin): Can this one be removed somehow?
      [ "ops" "nixos" ]
      [ "ops" "machines" "all-systems" ]
    ];
  };

  # Disallow access to //corp from other depot parts.
  corpFilter = restrictFolder {
    folder = "corp";
    reason = ''
      Code under //corp may use incompatible licensing terms with
      other depot parts and should not be used anywhere else.
    '';
  };

  readDepot = depotArgs: import ./nix/readTree {} {
    args = depotArgs;
    path = ./.;
    filter = parts: args: corpFilter parts (usersFilter parts args);
    scopedArgs = {
      __findFile = _: _: throw "Do not import from NIX_PATH in the depot!";
    };
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
      ++ concatMap gather (map (attr: node."${attr}") node.__readTreeChildren)
      # Include specified sub-targets of the node
      ++ filter eligible (map
           (k: (node."${k}" or {}) // {
             # Keep the same tree location, but explicitly mark this
             # node as a subtarget.
             __readTree = node.__readTree;
             __readTreeChildren = [];
             __subtarget = k;
           })
           (node.meta.targets or []))
    else [];

in fix(self: (readDepot {
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
}) // {
  # Make the path to the depot available for things that might need it
  # (e.g. NixOS module inclusions)
  path = self.third_party.nixpkgs.lib.cleanSource ./.;

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
