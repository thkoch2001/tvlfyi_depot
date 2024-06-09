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

    redshift.enable = true;
    blueman.enable = true;
    libinput.enable = true;

    xserver = {
      enable = true;
      xkb.layout = "us";
      xkb.options = "caps:super";

      displayManager = {
        # Give EXWM permission to control the session.
        sessionCommands = "${pkgs.xorg.xhost}/bin/xhost +SI:localuser:$USER";
        lightdm.enable = true;
        # lightdm.greeters.gtk.clock-format = "%H:%M"; # TODO(tazjin): TZ?
      };

      windowManager.session = lib.singleton {
        name = "exwm";
        start = "${config.tazjin.emacs}/bin/tazjins-emacs --internal-border=0 --border-width=0";
      };
      desktopManager.xfce.enable = true;
    };
  };

  # Set variables to enable EXWM-XIM and other Emacs features.
  environment.sessionVariables = {
    XMODIFIERS = "@im=exwm-xim";
    GTK_IM_MODULE = "xim";
    QT_IM_MODULE = "xim";
    CLUTTER_IM_MODULE = "xim";
    EDITOR = "emacsclient";
    _JAVA_AWT_WM_NONREPARENTING = "1";
  };

  # Do not restart the display manager automatically
  systemd.services.display-manager.restartIfChanged = lib.mkForce false;

  # If something needs more than 10s to stop it should probably be
  # killed.
  systemd.extraConfig = ''
    DefaultTimeoutStopSec=10s
  '';
}
