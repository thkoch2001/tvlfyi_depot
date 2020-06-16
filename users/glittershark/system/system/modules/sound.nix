{ config, lib, pkgs, ... }:
{
  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;
  nixpkgs.config.pulseaudio = true;

  environment.systemPackages = with pkgs; [
    pulseaudio-ctl
    paprefs
    pasystray
    pavucontrol
  ];
}
