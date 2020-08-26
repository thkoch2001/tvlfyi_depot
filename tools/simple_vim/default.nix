{ pkgs, ... }:

let
  configVim = builtins.path {
    path = ./config.vim;
    name = "config.vim";
  };

  script = pkgs.writeShellScriptBin "simple_vim" ''
    ${pkgs.vim}/bin/vim -u ${configVim}
  '';
in pkgs.stdenv.mkDerivation {
  name = "simple_vim";
  buildInputs = [ script ];
}
