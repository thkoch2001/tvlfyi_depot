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
}
