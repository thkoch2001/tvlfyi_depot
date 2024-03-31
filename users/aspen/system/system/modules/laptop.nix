{ config, lib, pkgs, ... }:

{
  services.logind = {
    powerKey = "hibernate";
    powerKeyLongPress = "poweroff";
    lidSwitch = "hybrid-sleep";
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
  };
}
