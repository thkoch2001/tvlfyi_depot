{ config, lib, pkgs, ... }:

{
  imports = [
    ./reusable/battery.nix
  ];

  laptop.onLowBattery.enable = true;

  services.logind.extraConfig = ''
    HandlePowerKey=hibernate
  '';

  services.tlp.enable = true;
}
