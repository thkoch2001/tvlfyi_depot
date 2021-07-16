# Emacs overlay from https://github.com/nix-community/emacs-overlay
{ ... }:

let
  # from 2021-07-24
  commit = "c57f50b4ab4768b50df501e7d5145806c596534e";
  src = builtins.fetchTarball {
    url = "https://github.com/nix-community/emacs-overlay/archive/${commit}.tar.gz";
    sha256 = "11pn5ixkcl5gqbkyimawn3n8klplnc9z7d193z57njkdlfs39jv4";
  };
in import src
