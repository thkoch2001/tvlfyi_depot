#! /usr/bin/env -S bash -c 'nix-shell -p crate2nix --run "crate2nix generate"; exec nix-build $0'

{ pkgs ? import <nixpkgs> { }
, lib ? pkgs.lib
}:

(import ./Cargo.nix { inherit pkgs; }).rootCrate.build.override {
  crateOverrides = pkgs.defaultCrateOverrides // {
    tvix-eval = attrs: {
      # If tvix-eval ever accrues native dependencies, add them
      # here.  You can also use this to tweak the build process:

      # nativeBuildInputs = [ ... ];
      # ENV_VAR = "something";
    };
  };
}
