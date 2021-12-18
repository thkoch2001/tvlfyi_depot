{ pkgs, ... }:

let
  configVim = builtins.path {
    path = ./config.vim;
    name = "config.vim";
  };

in
pkgs.writeShellScriptBin "simple_vim" ''
  ${pkgs.vim}/bin/vim -u ${configVim}
''
