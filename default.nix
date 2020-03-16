{ ... }:

let
  readTree = import <depot/nix/readTree> {} {
    pkgs      = import <nixpkgs> {};
    depot     = import <depot> {};
    briefcase = import <briefcase> {};
  };
in {
  nixos       = readTree ./nixos;
  utils       = readTree ./utils;
  emacs       = readTree ./emacs;
  learn       = readTree ./learn;
  blog        = readTree ./blog;
  lisp        = readTree ./lisp;
  gopkgs      = readTree ./gopkgs;
  monzo_ynab  = readTree ./monzo_ynab;
  third_party = readTree ./third_party;
  tools       = readTree ./tools;
}
