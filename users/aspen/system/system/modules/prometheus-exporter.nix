{ config, lib, pkgs, ... }:

with lib;

{
  services.prometheus.exporters = {
    node = {
      enable = true;
      openFirewall = false;

      enabledCollectors = [
        "processes"
        "systemd"
        "tcpstat"
        "wifi"
      ];
    };

    nginx = mkIf config.services.nginx.enable {
      enable = true;
      openFirewall = true;
      sslVerify = false;
      constLabels = [ "host=${config.networking.hostName}" ];
    };

    postgres = mkIf config.services.postgresql.enable {
      enable = true;
      runAsLocalSuperUser = true;
    };
  };
}
