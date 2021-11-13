# Emacs overlay from https://github.com/nix-community/emacs-overlay
{ ... }:

let
  # from 2021-11-14
  commit = "c75b7c047cc4635b0ecdedfd4ad78e1ac76e41c5";
  src = builtins.fetchTarball {
    url = "https://github.com/nix-community/emacs-overlay/archive/${commit}.tar.gz";
    sha256 = "06if42qy22xni3gd9hqqq6yhama20qn402asalj16mxi1rxx4kgz";
  };
in import src
