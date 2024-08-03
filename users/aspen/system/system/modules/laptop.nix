{ config, lib, pkgs, ... }:

{
  services.logind = {
    powerKey = "hibernate";
    powerKeyLongPress = "poweroff";
    lidSwitch = "suspend-then-hibernate";
    lidSwitchExternalPower = "ignore";
  };

  systemd.sleep.extraConfig = ''
    HibernateDelaySec=30m
    SuspendState=mem
  '';

  services.tlp.enable = true;

  services.upower = {
    enable = true;
    criticalPowerAction = "Hibernate";
    percentageAction = 3;
  };

  services.libinput = {
    touchpad = {
      naturalScrolling = true;
      disableWhileTyping = true;
    };
  };
}
