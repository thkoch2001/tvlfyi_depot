{ config, depot, lib, pkgs, ... }:

{
  systemd.services.tgsa = {
    description = "telegram -> SA bbcode thing";
    wantedBy = [ "multi-user.target" ];

    serviceConfig = {
      DynamicUser = true;
      Restart = "always";
      ExecStart = "${depot.users.tazjin.tgsa}/bin/tgsa";
    };
  };

  services.nginx.virtualHosts."tgsa" = {
    serverName = "tgsa.tazj.in";
    enableACME = true;
    forceSSL = true;

    locations."/" = {
      proxyPass = "http://localhost:8472";
    };
  };
}
