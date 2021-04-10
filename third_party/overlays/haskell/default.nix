# Defines an overlay for overriding Haskell packages, for example to
# avoid breakage currently present in nixpkgs or to modify package
# versions.

{ ... }: # This file needs nothing from readTree

self: super: # overlay parameters for the nixpkgs overlay

let
  overrides = hsSelf: hsSuper: with super.haskell.lib; rec {
    generic-arbitrary = appendPatch hsSuper.generic-arbitrary
      [ ./patches/generic-arbitrary-export-garbitrary.patch ];

    # check dependencies cause circular dependency on itself
    # and thus an infinite recursion
    random = dontCheck hsSuper.random_1_2_0;

    # random <1.2
    test-framework = doJailbreak hsSuper.test-framework;
    hashable = doJailbreak hsSuper.hashable;
    test-framework-quickcheck2 = doJailbreak hsSuper.test-framework-quickcheck2;

    # can be removed if we have the following PR or equivalent
    # https://github.com/NixOS/nixpkgs/pull/116931
    hedgehog-classes = overrideCabal hsSuper.hedgehog-classes (attrs: {
      # remove version bound on semirings which is inside a
      # conditional, so doJailbreak doesn't work
      prePatch = ''
        sed -i 's|semirings.*0.6|semirings|g' hedgehog-classes.cabal
      '';
    });

    hgeometry-combinatorial = dontCheck hsSuper.hgeometry-combinatorial;
  };
in {
  haskellPackages = super.haskellPackages.override {
    inherit overrides;
  };
}
