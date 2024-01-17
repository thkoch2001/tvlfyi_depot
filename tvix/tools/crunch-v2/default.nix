{ pkgs, ... }:

let
  crates = import ./Cargo.nix {
    inherit pkgs;
    nixpkgs = pkgs.path;

    defaultCrateOverrides = pkgs.defaultCrateOverrides // {
      crunch-v2 = prev: {
        nativeBuildInputs = (prev.nativeBuildInputs or [ ]) ++ [ pkgs.buildPackages.protobuf ];
      };
    };
  };
in crates.rootCrate.build
