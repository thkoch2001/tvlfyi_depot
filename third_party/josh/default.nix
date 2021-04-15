# https://github.com/esrlabs/josh
{ depot, pkgs, ... }:

let
  gitSrc = pkgs.fetchFromGitHub {
    owner = "esrlabs";
    repo = "josh";
    rev = "1c1965deb0278de92452a597c092f16b531274ae";
    sha256 = "0a0ybx4dqr9gfcn1kqy2nirdsfbdwkfz24ljnri564c5381ccldw";
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
  buildInputs = [ pkgs.libgit2 ];
}
