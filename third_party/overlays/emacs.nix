# Emacs overlay from https://github.com/nix-community/emacs-overlay
{ ... }:

let
  # from 2021-11-11
  commit = "72b8a1e5614b8ab2de30c7d25fa453e55152ac40";
  src = builtins.fetchTarball {
    url = "https://github.com/nix-community/emacs-overlay/archive/${commit}.tar.gz";
    sha256 = "1729nfci2vpc7yyfcan8dc2rw98amf2h5n1vrgm4nrbvyhyh3mv0";
  };
in import src
