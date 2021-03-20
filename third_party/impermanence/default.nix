# NixOS modules for systems with ephemeral root disks.
#
# https://github.com/nix-community/impermanence

{ pkgs, ... }:

pkgs.fetchFromGitHub {
  owner = "nix-community";
  repo = "impermanence";
  rev = "58558845bc68dcf2bb32caa80564f7fe3f6cbc61";
  sha256 = "10z3g4knkvq838zbfq71pkfyl8cffrpavna448wf5mjscycp0gnv";
}
