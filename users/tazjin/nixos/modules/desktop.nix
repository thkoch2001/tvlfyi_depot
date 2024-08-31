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
      enable = true; # wayland doesn't work otherwise ...?!
      displayManager.gdm = {
        enable = true;
        wayland = true;
      };
    };
  };

  services.displayManager.sessionPackages = [ pkgs.niri ];

  programs.xwayland.enable = true;

  environment.systemPackages = with pkgs; [
    # core packages
    niri
    xwayland-satellite
    swaylock

    # support tooling
    qt5.qtwayland
    sfwbar
    swayidle
    wdisplays
    wezterm
    wl-mirror
    xfce.xfce4-appfinder
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
