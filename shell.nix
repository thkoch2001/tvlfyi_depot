{ pkgs ? import <nixpkgs> { } }:

let
  nix' = pkgs.nix.overrideAttrs (final: prev: {
    src = pkgs.fetchFromGitHub {
      owner = "tweag";
      repo = "nix";
      rev = "nix-c-bindings";
      hash = "sha256-xOyU79lsz0THOj1LccfsDS45089n2DhlkWxaJFeKriY=";
    };
  });
in pkgs.buildNimPackage {
  name = "dummy";
  nativeBuildInputs = [ pkgs.pkg-config ];
  buildInputs = [ pkgs.boost nix' ];
  lockFile = ./lock.json;
}
