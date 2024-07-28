{ config, lib, pkgs, ... }:

{
  # Enable sound.
  hardware.pulseaudio.enable = true;

  environment.systemPackages = with pkgs; [
    pulseaudio-ctl
    paprefs
    pasystray
    pavucontrol
  ];

  hardware.pulseaudio.package = pkgs.pulseaudioFull;
}
