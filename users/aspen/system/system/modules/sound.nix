{ config, lib, pkgs, ... }:

{
  # Enable sound.
  hardware.pulseaudio.enable = true;
  services.pipewire.enable = false;

  environment.systemPackages = with pkgs; [
    pulseaudio-ctl
    paprefs
    pasystray
    pavucontrol
  ];

  hardware.pulseaudio.package = pkgs.pulseaudioFull;
}
