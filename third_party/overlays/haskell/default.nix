# Defines an overlay for overriding Haskell packages, for example to
# avoid breakage currently present in nixpkgs or to modify package
# versions.

{ ... }: # This file needs nothing from readTree

self: super: # overlay parameters for the nixpkgs overlay

let
  overrides = hsSelf: hsSuper: with super.haskell.lib; rec {
    generic-arbitrary = appendPatch hsSuper.generic-arbitrary
      [ ./patches/generic-arbitrary-export-garbitrary.patch ];

    # random = dontCheck (hsSuper.callHackageDirect {
    #   pkg = "random";
    #   ver = "1.2.0";
    #   sha256 = "06s3mmqbsfwv09j2s45qnd66nrxfp9280gnl9ng8yh128pfr7bjh";
    # } {});

    # random <1.2
    test-framework = doJailbreak hsSuper.test-framework;
    hashable = doJailbreak hsSuper.hashable;
    test-framework-quickcheck2 = doJailbreak hsSuper.test-framework-quickcheck2;
  };
in {
  haskellPackages = super.haskellPackages.override {
    inherit overrides;
  };
}
