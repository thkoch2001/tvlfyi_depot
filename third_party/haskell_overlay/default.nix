{ pkgs ? import ./nixpkgs.nix {} }:

self: super: with pkgs.haskell.lib; rec {
  generic-arbitrary = appendPatch
    super.generic-arbitrary
    [ ./patches/generic-arbitrary-export-garbitrary.patch ];

  hgeometry =
    appendPatch
      (self.callHackageDirect {
        pkg = "hgeometry";
        ver = "0.9.0.0";
        sha256 = "02hyvbqm57lr47w90vdgl71cfbd6lvwpqdid9fcnmxkdjbq4kv6b";
      } {}) [ ./patches/hgeometry-fix-haddock.patch ];

  hgeometry-combinatorial =
    self.callHackageDirect {
      pkg = "hgeometry-combinatorial";
      ver = "0.9.0.0";
      sha256 = "12k41wd9fd1y3jd5djwcpwg2s1cva87wh14i0m1yn49zax9wl740";
    } {};

  vinyl = overrideSrc (markUnbroken super.vinyl)
    rec {
      src = pkgs.fetchzip {
        url = "mirror://hackage/vinyl-${version}/vinyl-${version}.tar.gz";
        sha256 = "190ffrmm76fh8fi9afkcda2vldf89y7dxj10434h28mbpq55kgsx";
      };
      version = "0.12.0";
    };

  comonad-extras = appendPatch (markUnbroken super.comonad-extras)
    [ ./patches/update-comonad-extras.patch ];
}
