# This file sets up the top-level package set by traversing the package tree
# (see read-tree.nix for details) and constructing a matching attribute set
# tree.
#
# This makes packages accessible via the Nixery instance that is configured to
# use this repository as its nixpkgs source.

{ ... }@args:

with builtins;

let
  # This definition of fix is identical to <nixpkgs>.lib.fix, but the global
  # package set is not available here.
  fix = f: let x = f x; in x;

  # Global configuration that all packages are called with.
  config = depot: {
    inherit depot;

    # Expose lib & ciBuilds attributes to packages.
    inherit (depot) ciBuilds lib;

    # Pass third_party as 'pkgs' (for compatibility with external
    # imports for certain subdirectories)
    pkgs = depot.third_party;
  };

  readTree' = import ./nix/readTree {};

  localPkgs = readTree: {
    fun           = readTree ./fun;
    lisp          = readTree ./lisp;
    net           = readTree ./net;
    nix           = readTree ./nix;
    ops           = readTree ./ops;
    third_party   = readTree ./third_party;
    tools         = readTree ./tools;
    users         = readTree ./users;
    web           = readTree ./web;
  };

  # To determine build targets, we walk through the depot tree and
  # fetch attributes that were imported by readTree and are buildable.
  #
  # Any build target that contains `meta.ci = false` will be skipped.

  # Is this tree node eligible for build inclusion?
  eligible = node: (node ? outPath) && (node.meta.ci or true);

  # Walk the tree starting with 'node', recursively extending the list
  # of build targets with anything that looks buildable.
  gather = node:
    if node ? __readTree then
      (if eligible node then [node] else []) ++
      concatMap gather (attrValues node)
    else [];
in fix(self: {
  config = config self;

  # Elevate 'lib' from nixpkgs
  lib = import (self.third_party.nixpkgsSrc + "/lib");

  # Expose readTree for downstream repo consumers.
  readTree = {
    __functor = x: (readTree' x.config);
    config = self.config;
  };

  # Make the path to the depot available for things that might need it
  # (e.g. NixOS module inclusions)
  depotPath = ./.;

  # List of all buildable targets, for CI purposes.
  #
  # Note: This *must* be a nested attribute, otherwise we will get
  # infinite recursion and everything blows up.
  ci.targets = gather self;
}

# Add local packages as structured by readTree
// (localPkgs (readTree' self.config))

# Load overrides into the top-level.
#
# This can be used to move things from third_party into the top-level, too (such
# as `lib`).
// (readTree' { depot = self; pkgs = self.third_party; }) ./overrides
)
