# Experimental configuration for manually Livegrep.
{ config, ... }:

{
  imports = [ ./base.nix ];

  config = {
    services.nginx.virtualHosts."grep.tvl.fyi" = {
      enableACME = true;
      forceSSL = true;

      locations."/" = {
        proxyPass = "http://127.0.0.1:${toString config.services.depot.livegrep.port}";
      };
    };
  };
}
