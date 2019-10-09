{ pkgs ? import (builtins.fetchTarball
  "https://github.com/tazjin/depot/archive/master.tar.gz") {} }:

pkgs.writeElispBin {
  name = "string";
  deps = epkgs: [ epkgs.dash epkgs.s ./prelude.nix ];
  src = ./string.el;
}
