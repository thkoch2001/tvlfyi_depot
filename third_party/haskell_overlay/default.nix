{ pkgs ? import ./nixpkgs.nix {}, ... }:

self: super: with pkgs.haskell.lib; rec {
  aeson = doJailbreak super.aeson;

  attoparsec = doJailbreak super.attoparsec;

  cassava = doJailbreak super.cassava;

  fgl = overrideSrc (doJailbreak super.fgl) rec {
    src = pkgs.fetchzip {
      url = "mirror://hackage/fgl-${version}/fgl-${version}.tar.gz";
      sha256 = "0spyd56b2rmwp8n6h167rfjwy8lpcvar1p2rqhw9q580h2l9v61l";
    };
    version = "5.7.0.3";
  };

  fgl-arbitrary = overrideSrc super.fgl-arbitrary rec {
    src = pkgs.fetchzip {
      url = "mirror://hackage/fgl-arbitrary-${version}/fgl-arbitrary-${version}.tar.gz";
      sha256 = "0bacdv51am13x5k64xvmxcs5gkdkh0gpmnh05wgd2vqg1n8r7fwb";
    };
    version = "0.2.0.6";
  };

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

  psqueues = doJailbreak super.psqueues;

  random = dontCheck (self.callHackageDirect {
    pkg = "random";
    ver = "1.2.0";
    sha256 = "06s3mmqbsfwv09j2s45qnd66nrxfp9280gnl9ng8yh128pfr7bjh";
  } {});

  random-source = overrideSrc super.random-source rec {
    src = pkgs.fetchzip {
      url = "mirror://hackage/random-source-${version}/random-source-${version}.tar.gz";
      sha256 = "0yx0i4fv1xg535zd2isczfngsq5740wzl6w44168lxdmpw9fmsyz";
    };
    # downgraded as the latest version was causing a ghc crash:
    # https://gitlab.haskell.org/ghc/ghc/-/issues/18537
    version = "0.3.0.8";
  };

  semialign = self.callHackageDirect {
    pkg = "semialign";
    ver = "1.1.0.1";
    sha256 = "1xs5dvz87gx6xnannw6bc70nzr8ffhk0j6n7n0p5dqair9sz77x4";
  } {};

  splitmix = dontCheck (self.callHackageDirect {
    pkg = "splitmix";
    ver = "0.1";
    sha256 = "1k7l07h2w4fhjdqiqvw48if0irx0ngv6niach265j7lbfxsm8qql";
  } {});

  hedgehog = doJailbreak super.hedgehog;

  hspec-core = dontCheck super.hspec-core;

  QuickCheck = overrideSrc super.QuickCheck rec {
    src = pkgs.fetchzip {
      url = "mirror://hackage/QuickCheck-${version}/QuickCheck-${version}.tar.gz";
      sha256 = "0x9pnr9m81jlywj38w3530zw1g0xmfszmj9303m2fp58zpad96h0";
    };
    version = "2.14.1";
  };

  test-framework-quickcheck2 = doJailbreak super.test-framework-quickcheck2;

  vector = overrideSrc (doJailbreak super.vector) rec {
    src = pkgs.fetchzip {
      url = "mirror://hackage/vector-${version}/vector-${version}.tar.gz";
      sha256 = "1312lpb1f4jzbmcjp7mdf9l9ykp1hscxdr66cl8zlcs8kbr13bm7";
    };
    version = "0.12.1.2";
  };

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
