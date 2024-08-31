# EXWM and other desktop configuration.
{ config, depot, lib, pkgs, ... }:

{
  services = {
    pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
    };

    blueman.enable = true;
    libinput.enable = true;

    xserver = {
      enable = true;
      xkb.layout = "us";
      xkb.options = "caps:super";

      displayManager.sessionPackages = [ pkgs.niri ];
      displayManager.gdm = {
        enable = true;
        wayland = true;
      };
    };
  };

  programs.xwayland.enable = true;

  environment.systemPackages = with pkgs; [
    niri
    wezterm
    xwayland-satellite
    swaybg
    swaylock
  ];

  # Do not restart the display manager automatically
  systemd.services.display-manager.restartIfChanged = lib.mkForce false;

  # swaylock needs an empty PAM configuration, otherwise it locks the user out
  security.pam.services.swaylock = { };

  # If something needs more than 10s to stop it should probably be
  # killed.
  systemd.extraConfig = ''
    DefaultTimeoutStopSec=10s
  '';
}
