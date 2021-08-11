# Emacs overlay from https://github.com/nix-community/emacs-overlay
{ ... }:

let
  # from 2021-08-11
  commit = "3fbe6cf3459cf955e188444b68f085f7a45b6ffa";
  src = builtins.fetchTarball {
    url = "https://github.com/nix-community/emacs-overlay/archive/${commit}.tar.gz";
    sha256 = "0hm7lkqxh08nq074fgn0naqr6lg12qqxwsr3i4wpvdskmdbhb5i5";
  };
in import src
