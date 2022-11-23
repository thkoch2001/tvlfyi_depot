#! /usr/bin/env -S bash -c 'nix-shell -p crate2nix --run "crate2nix generate"; exec nix-build $0'
{ pkgs ? import <nixpkgs> { }
, lib ? pkgs.lib
}:

(import ./Cargo.nix { inherit pkgs; }).rootCrate.build.override {
  crateOverrides = pkgs.defaultCrateOverrides // {
    tvix-store = attrs: {
      nativeBuildInputs = [ pkgs.protobuf ];
      PROTOC = "${lib.getBin pkgs.protobuf}/bin/protoc";
    };
  };
}
