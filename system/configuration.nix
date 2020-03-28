{ config, pkgs, ... }:

let machine = throw "Pick a machine from ./machines"; in
{
  imports =
    [
      /etc/nixos/hardware-configuration.nix
      ./modules/common.nix
      machine
    ];
}
