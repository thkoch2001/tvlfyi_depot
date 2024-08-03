{ config, pkgs, ... }:
{
  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    xkb.layout = "us";


    windowManager.i3.enable = true;
  };

  servces.displayManager.defaultSession = "none+i3";

  services.libinput.enable = true;
}
