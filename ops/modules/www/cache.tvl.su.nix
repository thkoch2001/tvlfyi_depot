{ config, ... }:

{
  imports = [ ./base.nix ];

  config = {
    services.nginx.virtualHosts."cache.tvl.su" = {
      serverName = "cache.tvl.su";
      serverAliases = [ "cache.tvl.fyi" ];
      enableACME = true;
      forceSSL = true;

      extraConfig = ''
        location = /cache-key.pub {
          alias /run/agenix/nix-cache-pub;
        }

        location = /nix-cache-info {
          add_header Content-Type text/plain;
          return 200 "StoreDir: /nix/store\nWantMassQuery: 1\nPriority: 50\n";
        }

        location / {
          proxy_pass http://localhost:${toString config.services.nix-serve.port};
        }
      '';
    };
  };
}
