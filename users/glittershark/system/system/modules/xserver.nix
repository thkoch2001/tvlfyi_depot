{ config, pkgs, ... }:
{
  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    layout = "us";

    libinput.enable = true;

    displayManager = {
      defaultSession = "none+i3";
    };

    windowManager.i3.enable = true;
  };
}
