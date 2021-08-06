# Emacs overlay from https://github.com/nix-community/emacs-overlay
{ ... }:

let
  # from 2021-08-06
  commit = "e37266bc538b0562b9c1ffb3618ba265cb6768e9";
  src = builtins.fetchTarball {
    url = "https://github.com/nix-community/emacs-overlay/archive/${commit}.tar.gz";
    sha256 = "05m13nw5chmd9gnrxz2c4yndp3w4pf4rrci84yk99zkijmkvsd6i";
  };
in import src
