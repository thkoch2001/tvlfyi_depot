{ config, ... }:

{
  imports = [
    ./base.nix
  ];

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

        location / {
          proxy_pass http://localhost:${toString config.services.nix-serve.port};
        }
      '';
    };
  };
}
