#! /usr/bin/env nix-shell
#! nix-shell --run "crate2nix generate" -p crate2nix
#! nix-shell -i "nix-build"

{ pkgs ? import <nixpkgs> {}
, lib ? pkgs.lib }:

(import ./Cargo.nix { inherit pkgs; })
  .rootCrate.build.override {
/*
    crateOverrides = pkgs.defaultCrateOverrides // {
      tvix-store = attrs: {
        nativeBuildInputs = [ pkgs.protobuf ];
        PROTOC = "${lib.getBin pkgs.protobuf}/bin/protoc";
      };
    };
*/
  }
