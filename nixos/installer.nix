{ config, pkgs, ...  }:

{
  imports = [
    <nixpkgs/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix>
  ];
  config = {
    networking.wireless.enable = true;
    networking.wireless.networks."GoogleGuest" = {};
  };
}
