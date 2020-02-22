{ pkgs ? import <nixpkgs> {}, ... }:

pkgs.writeShellScriptBin "rebuild" ''
  set -ue
  sudo nixos-rebuild \
    -I nixos-config=/home/wpcarro/briefcase/nixos/configuration.nix \
    -I nixpkgs=/home/wpcarro/nixpkgs \
    switch
''
