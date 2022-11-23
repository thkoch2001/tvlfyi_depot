# To use this, run:
#
#   nix-shell -p crate2nix --command 'crate2nix generate'
#   nix-build crate.nix
#

{ pkgs ? import <nixpkgs> {}
, lib ? pkgs.lib }:

(import ./Cargo.nix { inherit pkgs; })
  .rootCrate.build.override {
    crateOverrides = pkgs.defaultCrateOverrides // {
      tvix-store = attrs: {
        nativeBuildInputs = [ pkgs.protobuf ];
        PROTOC = "${lib.getBin pkgs.protobuf}/bin/protoc";
      };
    };
  }
