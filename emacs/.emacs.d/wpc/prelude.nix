{ pkgs ? import (builtins.fetchTarball
  "https://github.com/tazjin/depot/archive/master.tar.gz") {} }:

# Ciruclar dependency warning: list.nix depends on prelude.nix, which depends on
# list.nix.
pkgs.writeElispBin {
  name = "prelude";
  # If this can build with epkgs.ht, remove `(require 'ht)`.
  deps = epkgs: [ epkgs.s epkgs.dash epkgs.f ./string.nix ./list.nix ];
  src = ./prelude.el;
}
