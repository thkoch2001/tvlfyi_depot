{ pkgs, ... }:

(pkgs.callPackage ./pkg.nix { }).overrideAttrs (old: {
  passthru = old.passthru // {
    module = ./module.nix;
  };
})
