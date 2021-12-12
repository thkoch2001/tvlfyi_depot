{ config, ... }:

{
  imports = [ ./base.nix ];

  config = {
    services.nginx.virtualHosts."status-fyi" = {
      serverName = "status.tvl.fyi";
      enableACME = true;
      extraConfig = "return 302 https://status.tvl.su$request_uri;";
    };

    services.nginx.virtualHosts.grafana = {
      serverName = "status.tvl.su";
      enableACME = true;
      forceSSL = true;

      locations."/" = {
        proxyPass = "http://localhost:${toString config.services.grafana.port}";
      };
    };
  };
}
