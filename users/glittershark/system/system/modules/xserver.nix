{ config, pkgs, ... }:
{
  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    layout = "us";

    libinput.enable = true;

    displayManager = {
      defaultSession = "none+i3";

      autoLogin = {
        enable = true;
        user = "grfn";
      };
    };

    windowManager.i3.enable = true;
#       enable = true;
#       extraPackages = with pkgs; [
#         i3status
#         i3lock
#       ];
#     };
  };
}
