# Emacs overlay from https://github.com/nix-community/emacs-overlay
{ ... }:

let
  # from 2021-07-25
  commit = "850ddaaadd4c640bd8eb4b154734a96bbe234d07";
  src = builtins.fetchTarball {
    url = "https://github.com/nix-community/emacs-overlay/archive/${commit}.tar.gz";
    sha256 = "178ysnqs6k7y8h9zvd3l7c0m6lqzncn3dh0fb8sqv4hgsvx95j6r";
  };
in import src
