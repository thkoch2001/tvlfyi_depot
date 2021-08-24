# Emacs overlay from https://github.com/nix-community/emacs-overlay
{ ... }:

let
  # from 2021-08-24
  commit = "b57f95c78042800bbd2b6b3c6c1f9fb85221507e";
  src = builtins.fetchTarball {
    url = "https://github.com/nix-community/emacs-overlay/archive/${commit}.tar.gz";
    sha256 = "17xv9c3x5930zy6hbqk20pwk601w5xnrzpg5gsnrkzl1z726qrn1";
  };
in import src
