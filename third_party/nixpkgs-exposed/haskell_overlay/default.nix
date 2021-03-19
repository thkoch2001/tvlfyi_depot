{ pkgs }:

self: super: with pkgs.haskell.lib; rec {
  generic-arbitrary = appendPatch
    super.generic-arbitrary
    [ ./patches/generic-arbitrary-export-garbitrary.patch ];

  random = dontCheck (self.callHackageDirect {
    pkg = "random";
    ver = "1.2.0";
    sha256 = "06s3mmqbsfwv09j2s45qnd66nrxfp9280gnl9ng8yh128pfr7bjh";
  } {});

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

  # pin hgeometry* to 0.11.0.0 since 0.12.0.0 removes triangulationEdges
  # which is used by //users/glittershark/xanthous
  hgeometry =
    self.callHackageDirect {
      pkg = "hgeometry";
      ver = "0.11.0.0";
      sha256 = "0qidbpgs6jxrirrhmy7iabwd62178sm68fqrmqg3w3gfyx8nm8ls";
    } {};

  hgeometry-combinatorial =
    self.callHackageDirect {
      pkg = "hgeometry-combinatorial";
      ver = "0.11.0.0";
      sha256 = "0c9ccqz1m45kkdkzw00gvzdspjljhg12vish6himqjqpms7g6sag";
    } {};
}
