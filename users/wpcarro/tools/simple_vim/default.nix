{ pkgs, ... }:

pkgs.writeShellScriptBin "simple_vim" ''
  ${pkgs.neovim}/bin/nvim -u ${configVim} $@
''
