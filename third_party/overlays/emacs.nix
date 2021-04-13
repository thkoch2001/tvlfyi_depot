# Emacs overlay from https://github.com/nix-community/emacs-overlay
{ ... }:

let
  # from 2020-04-13
  commit = "15ed1f372a83ec748ac824bdc5b573039c18b82f";
  src = builtins.fetchTarball {
    url = "https://github.com/nix-community/emacs-overlay/archive/${commit}.tar.gz";
    sha256 = "0m4vb7p29rgbpaavwn9jjid1zz48k1l9za5gy3d8nadqjn7x4dm1";
  };
in import src
