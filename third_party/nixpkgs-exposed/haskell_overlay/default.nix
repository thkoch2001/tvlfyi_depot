{ pkgs }:

self: super: with pkgs.haskell.lib; rec {
  generic-arbitrary = appendPatch
    super.generic-arbitrary
    [ ./patches/generic-arbitrary-export-garbitrary.patch ];

  # random = super.random_1_2_0 causes infinite recursion
  # for some reason, to investigate if it is avoidable.
  random = overrideCabal super.random (random_1_1: {
    # change sources to 1.2.0
    src = pkgs.fetchurl {
      url = "https://hackage.haskell.org/package/random-1.2.0/random-1.2.0.tar.gz";
      sha256 = "1pmr7zbbqg58kihhhwj8figf5jdchhi7ik2apsyxbgsqq3vrqlg4";
    };
    version = "1.2.0";
    # don't fetch updated cabal file, jailbreak instead
    editedCabalFile = null;
    jailbreak = true;
    # get rid of test deps, add new dependency splitmix
    doCheck = false;
    libraryHaskellDepends = [
      self.splitmix
    ] ++ random_1_1.libraryHaskellDepends;
  });

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
