# Emacs overlay from https://github.com/nix-community/emacs-overlay
{ ... }:

let
  # from 2021-10-10
  commit = "de51ad003df13d41a1107a1ed766eb226ef9382c";
  src = builtins.fetchTarball {
    url = "https://github.com/nix-community/emacs-overlay/archive/${commit}.tar.gz";
    sha256 = "1spfd7wx7lz48kk7cs91gjx6zhvlliqc6376rxkc4idfb0q43xih";
  };
in import src
