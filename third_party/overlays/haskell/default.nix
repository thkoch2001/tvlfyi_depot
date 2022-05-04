# Defines an overlay for overriding Haskell packages, for example to
# avoid breakage currently present in nixpkgs or to modify package
# versions.

{ lib, ... }:

self: super: # overlay parameters for the nixpkgs overlay

let
  overrides = hsSelf: hsSuper: with self.haskell.lib.compose; {
    # No overrides for the default package set necessary at the moment
  };
in
{
  haskellPackages = super.haskellPackages.override {
    inherit overrides;
  };

  haskell = lib.recursiveUpdate super.haskell {
    packages.ghc8107 = super.haskell.packages.ghc8107.override {
      overrides = lib.composeExtensions overrides (
        hsSelf: hsSuper: with self.haskell.lib.compose; {
          # TODO(sterni): TODO(grfn): patch xanthous to work with random-fu 0.3.*,
          # so we can use GHC 9.0.2 and benefit from upstream binary cache.
          random-fu = hsSelf.callPackage ./extra-pkgs/random-fu-0.2.nix { };
          rvar = hsSelf.callPackage ./extra-pkgs/rvar-0.2.nix { };
        }
      );
    };
  };
}
