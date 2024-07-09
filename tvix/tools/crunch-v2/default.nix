{ pkgs, depot, lib, ... }:

(pkgs.callPackage ./Cargo.nix {
  defaultCrateOverrides = (depot.tvix.utils.defaultCrateOverridesForPkgs pkgs) // {
    crunch-v2 = prev: {
      src = depot.tvix.utils.filterRustCrateSrc rec {
        root = prev.src.origSrc;
        extraFileset = lib.fileset.fileFilter (f: f.hasExt "proto") root;
      };
      nativeBuildInputs = [ pkgs.protobuf ];
    };
  };
}).rootCrate.build
