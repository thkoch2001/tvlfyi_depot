{ pkgs ? import <nixpkgs> { } }:

let
  nix' = pkgs.nix.overrideAttrs (final: prev: {
    src = pkgs.fetchFromGitHub {
      owner = "NixOS";
      repo = "nix";
      rev = "2.13.3";
      hash = "sha256-jUc2ccTR8f6MGY2pUKgujm+lxSPNGm/ZAP+toX+nMNc=";
    };
  });
in pkgs.nim2Packages.buildNimPackage {
  name = "dummy";
  nativeBuildInputs = [ pkgs.pkg-config ];
  buildInputs = [ pkgs.boost nix' ];
}
