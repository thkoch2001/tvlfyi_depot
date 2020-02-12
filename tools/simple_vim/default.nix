{ pkgs ? import <nixpkgs> {}, ... }:

let
  script = pkgs.writeShellScriptBin "simple_vim" ''
    ${pkgs.vim}/bin/vim -u ${./config.vim}
  '';
in pkgs.stdenv.mkDerivation {
  name = "simple_vim";
  buildInputs = [ script ];
}
