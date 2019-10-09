{ pkgs ? import (builtins.fetchTarball
  "https://github.com/tazjin/depot/archive/master.tar.gz") {} }:

pkgs.writeElispBin {
  name = "list";
  deps = epkgs: [ epkgs.dash ./prelude.nix ];
  src = ./list.el;
}
