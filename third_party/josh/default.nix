# https://github.com/esrlabs/josh
{ depot, pkgs, ... }:

let
  gitSrc = pkgs.fetchFromGitHub {
    owner = "esrlabs";
    repo = "josh";
    rev = "92fd7fb193d0df25b106207675f233cfe1d26d0d";
    sha256 = pkgs.lib.fakeSha256;
  };

  src = pkgs.applyPatches {
    name = "josh-src";
    src = gitSrc;
    patches = [
      ./pin-git2-rev.patch
    ];
  };
in depot.third_party.naersk.buildPackage {
  inherit src;

  buildInputs = with pkgs; [
    libgit2
    openssl
    pkgconfig
  ];

  cargoBuildOptions = x: x ++ [
    "-p" "josh"
    "-p" "josh-proxy"
    "-p" "josh-ui"
  ];
}
