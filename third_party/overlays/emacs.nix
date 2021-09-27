# Emacs overlay from https://github.com/nix-community/emacs-overlay
{ ... }:

let
  # from 2021-09-27
  commit = "61252f6d58c14eae1d67724c515bc27006df7999";
  src = builtins.fetchTarball {
    url = "https://github.com/nix-community/emacs-overlay/archive/${commit}.tar.gz";
    sha256 = "1d8869ld8zfq5q1gwx2iknikyymf6nfazrcm9agis1lab22ppp8r";
  };
in import src
