{ pkgs, ... }:

pkgs.writeShellScriptBin "rebuild" ''
  set -ue
  sudo nixos-rebuild \
    -I nixos-config=/home/wpcarro/briefcase/nixos/socrates/default.nix \
    -I nixpkgs=/home/wpcarro/nixpkgs \
    -I depot=/home/wpcarro/depot \
    -I briefcase=/home/wpcarro/briefcase \
    switch
''
