{ config, ... }:

{
  imports = [ ./base.nix ];

  config = {
    services.nginx.virtualHosts."cs.tvl.fyi" = {
      serverName = "cs.tvl.fyi";
      serverAliases = [ "cs.tvl.su" ];
      enableACME = true;
      forceSSL = true;

      extraConfig = ''
        location = / {
          return 301 https://cs.tvl.fyi/depot;
        }

        location / {
          proxy_set_header X-Sg-Auth "Anonymous";
          proxy_pass http://localhost:${
            toString config.services.depot.sourcegraph.port
          };
        }

        location /users/Anonymous/settings {
          return 301 https://cs.tvl.fyi;
        }
      '';
    };
  };
}
