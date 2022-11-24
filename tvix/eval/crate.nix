#! /usr/bin/env nix-shell
#! nix-shell --run "crate2nix generate" -p crate2nix
#! nix-shell -i "nix-build"

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
