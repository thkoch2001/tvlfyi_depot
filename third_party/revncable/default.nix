{ depot, ... }:

let src = pkgs.fetchFromGitHub {
  owner = "sandsmark";
  repo = "revncable";
  rev = "0c0aa4188c119cc26cc31c067c97526bf6101b0a";
  hash = "sha256:0gl5w8vgji0s6415nlrpzv96vn1in6hg1gwxyypxbbl3h3csv4gz";
};
libqsgepaper = builtins.fetchurl "https://toltec.delab.re/thirdparty/lib/libqsgepaper.a";
pkgs = import depot.third_party.nixpkgsSrc {
  overlays = [ (self: super: {
    libthai = super.libthai.overrideAttrs(old: {
      configureFlags = ["--disable-dict"];
    });
  })];
};
in pkgs.pkgsCross.remarkable1.libsForQt5.callPackage (
  { stdenv, qmake }:
  stdenv.mkDerivation {
    pname = "revncable";
    version = "0.0.1";
    src = ./.;

    preBuild = ''
      ln -s ${libqsgepaper} libqsgepaper.a
    '';

    nativeBuildInputs = [
      qmake
    ];
  }
) {}
