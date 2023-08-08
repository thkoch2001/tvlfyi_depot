# Users of this package & module should replace it with something like
# inadyn, after https://github.com/NixOS/nixpkgs/issues/242330 is
# landed.
#
# TODO(aspen): replace ddclient with inadyn or something else.
{ pkgs, ... }:

(pkgs.callPackage ./pkg.nix { }).overrideAttrs (old: {
  passthru = old.passthru // {
    module = ./module.nix;
  };
})
