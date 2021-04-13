# Emacs overlay from https://github.com/nix-community/emacs-overlay
{ ... }:

let
  # from 2020-05-26
  commit = "5df3462dda05d8e44669cf374776274e1bc47d0a";
  src = builtins.fetchTarball {
    url = "https://github.com/nix-community/emacs-overlay/archive/${commit}.tar.gz";
    sha256 = "0ggmkg4shf9948wpwb0s40bjvwijvhv2wykrkayclvp419kbrfxq";
  };
in import src
