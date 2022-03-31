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
      overrides = hsSelf: hsSuper: with self.haskell.lib.compose; {
        # TODO(sterni): TODO(grfn): patch xanthous to work with random-fu 0.3.*
        # We could then use the default GHC and wouldn't need it at eval time.
        random-fu = hsSelf.callHackage "random-fu" "0.2.7.7" {};
        rvar = hsSelf.callHackage "rvar" "0.2.0.6" {};

        # TODO(sterni): upstream this
        universe-base = addBuildDepend hsSelf.OneTuple hsSuper.universe-base;
      };
    };
  };
}
