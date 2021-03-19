{ pkgs }:

self: super: with pkgs.haskell.lib; rec {
  generic-arbitrary = appendPatch
    super.generic-arbitrary
    [ ./patches/generic-arbitrary-export-garbitrary.patch ];

  # TODO(sterni) let ci figure this out
  # hgeometry =
  #   appendPatch
  #     (self.callHackageDirect {
  #       pkg = "hgeometry";
  #       ver = "0.9.0.0";
  #       sha256 = "02hyvbqm57lr47w90vdgl71cfbd6lvwpqdid9fcnmxkdjbq4kv6b";
  #     } {}) [ ./patches/hgeometry-fix-haddock.patch ];

  # hgeometry-combinatorial =
  #   self.callHackageDirect {
  #     pkg = "hgeometry-combinatorial";
  #     ver = "0.9.0.0";
  #     sha256 = "12k41wd9fd1y3jd5djwcpwg2s1cva87wh14i0m1yn49zax9wl740";
  #   } {};

  random = dontCheck (self.callHackageDirect {
    pkg = "random";
    ver = "1.2.0";
    sha256 = "06s3mmqbsfwv09j2s45qnd66nrxfp9280gnl9ng8yh128pfr7bjh";
  } {});

  # random <1.2
  test-framework = doJailbreak super.test-framework;
  hashable = doJailbreak super.hashable;
  test-framework-quickcheck2 = doJailbreak super.test-framework-quickcheck2;

  # TODO(sterni) let ci figure this out
  # vinyl = overrideSrc (markUnbroken super.vinyl)
  #   rec {
  #     src = pkgs.fetchzip {
  #       url = "mirror://hackage/vinyl-${version}/vinyl-${version}.tar.gz";
  #       sha256 = "190ffrmm76fh8fi9afkcda2vldf89y7dxj10434h28mbpq55kgsx";
  #     };
  #     version = "0.12.0";
  #   };
}
