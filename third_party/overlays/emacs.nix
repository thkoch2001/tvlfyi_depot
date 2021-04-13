# Emacs overlay from https://github.com/nix-community/emacs-overlay
{ ... }:

let
  # from 2020-04-13
  commit = "15ed1f372a83ec748ac824bdc5b573039c18b82f";
  url = "https://github.com/nix-community/emacs-overlay/archive/${commit}.tar.gz";
in import (builtins.fetchTarball url)
