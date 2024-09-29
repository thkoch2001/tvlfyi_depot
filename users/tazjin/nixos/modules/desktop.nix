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
    alacritty
    fuzzel
    qt5.qtwayland
    swayidle
    waybar
    wdisplays
    wl-mirror
    xfce.xfce4-appfinder
    depot.users.tazjin.niri-reap
  ];

  # Do not restart the display manager automatically
  systemd.services.display-manager.restartIfChanged = lib.mkForce false;

  # pipewire MUST start before niri, otherwise screen sharing doesn't work
  systemd.user.services.pipewire.wantedBy = [ "niri.service" ];
  systemd.user.services.pipewire.before = [ "niri.service" ];

  # enable "desktop portals", which are important somehow
  xdg.portal = {
    enable = true;
    extraPortals = with pkgs; [
      xdg-desktop-portal-gtk
      xdg-desktop-portal-gnome
    ];
    config.common.default = "*";
  };

  # swaylock needs an empty PAM configuration, otherwise it locks the user out
  security.pam.services.swaylock = { };

  # enable theming support for Qt that is compatible with Chicago95 theme
  qt.enable = true;
  qt.platformTheme = "qt5ct";

  # If something needs more than 10s to stop it should probably be
  # killed.
  systemd.extraConfig = ''
    DefaultTimeoutStopSec=10s
  '';
}
