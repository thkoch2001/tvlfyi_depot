# EXWM and other desktop configuration.
{ depot, lib, pkgs, ... }:

{
  services = {
    pipewire = {
      enable = true;
      alsa.enable = true;
      pulse.enable = true;
    };

    redshift.enable = true;
    blueman.enable = true;

    xserver = {
      enable = true;
      layout = "us";
      xkbOptions = "caps:super";
      updateDbusEnvironment = true;
      libinput.enable = true;

      displayManager = {
        # Give EXWM permission to control the session.
        sessionCommands = "${pkgs.xorg.xhost}/bin/xhost +SI:localuser:$USER";
        lightdm.enable = true;
        # lightdm.greeters.gtk.clock-format = "%H:%M"; # TODO(tazjin): TZ?
      };

      windowManager.session = lib.singleton {
        name = "exwm";
        start = "${depot.users.tazjin.emacs}/bin/tazjins-emacs";
      };
    };
  };

  services.dbus.packages = [ pkgs.kbdd ];

  # Set variables to enable EXWM-XIM and other Emacs features.
  environment.sessionVariables = {
    XMODIFIERS = "@im=exwm-xim";
    GTK_IM_MODULE = "xim";
    QT_IM_MODULE = "xim";
    CLUTTER_IM_MODULE = "xim";
    EDITOR = "emacsclient";
  };

  # Do not restart the display manager automatically
  systemd.services.display-manager.restartIfChanged = lib.mkForce false;

  # If something needs more than 10s to stop it should probably be
  # killed.
  systemd.extraConfig = ''
    DefaultTimeoutStopSec=10s
  '';
}
