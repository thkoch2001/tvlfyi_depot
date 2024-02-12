{ config, lib, pkgs, ... }:
{
  imports = [
    ../modules/reusable/battery.nix
  ];

  networking.hostName = "bumblebee";

  powerManagement = {
    enable = true;
    cpuFreqGovernor = "powersave";
    powertop.enable = true;
  };

  # Hibernate on low battery
  laptop.onLowBattery = {
    enable = true;
    action = "hibernate";
    thresholdPercentage = 5;
  };

  services.xserver.xkbOptions = "caps:swapescape";
}
