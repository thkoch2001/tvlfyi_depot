# Defines overrides for Haskell packages, for example to avoid
# breakage currently present in nixpkgs or to modify package versions.

{ ... }: # This file needs nothing from readTree
{ pkgs }: # ... but is called with a separate package set in the overlay

self: super: with pkgs.haskell.lib; rec {
  generic-arbitrary = appendPatch
    super.generic-arbitrary
    [ ./patches/generic-arbitrary-export-garbitrary.patch ];

  # check dependencies cause circular dependency on itself
  # and thus an infinite recursion
  random = dontCheck super.random_1_2_0;

  # random <1.2
  test-framework = doJailbreak super.test-framework;
  hashable = doJailbreak super.hashable;
  test-framework-quickcheck2 = doJailbreak super.test-framework-quickcheck2;

  # can be removed if we have the following PR or equivalent
  # https://github.com/NixOS/nixpkgs/pull/116931
  hedgehog-classes = overrideCabal super.hedgehog-classes (attrs: {
    # remove version bound on semirings which is inside a
    # conditional, so doJailbreak doesn't work
    prePatch = ''
      sed -i 's|semirings.*0.6|semirings|g' hedgehog-classes.cabal
    '';
  });

  hgeometry-combinatorial = dontCheck super.hgeometry-combinatorial;
}
