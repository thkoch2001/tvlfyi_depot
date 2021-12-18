# Emacs overlay from https://github.com/nix-community/emacs-overlay
{ ... }:

let
  # from 2021-12-07
  commit = "2f14e98a92505c517a8364c3a5e614592fbea4f4";
  src = builtins.fetchTarball {
    url = "https://github.com/nix-community/emacs-overlay/archive/${commit}.tar.gz";
    sha256 = "16i16pm3bm43fj85n5bafghak0asi7p9xpyshpls6yyh85chcdb5";
  };
in
import src
