# Emacs overlay from https://github.com/nix-community/emacs-overlay
{ ... }:

let
  # from 2021-10-22
  commit = "e594a3e8a7d0fbed07a4ed61a7b3eb8f15ece547";
  src = builtins.fetchTarball {
    url = "https://github.com/nix-community/emacs-overlay/archive/${commit}.tar.gz";
    sha256 = "1qqph536mw60cvyzick2bzrampjm840nf3vb84mmmk6sdqlhglwf";
  };
in import src
