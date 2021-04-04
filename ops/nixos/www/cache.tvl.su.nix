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
        location /cache-key.pub {
          alias /etc/secrets/nix-cache-key.pub;
        }

        location / {
          proxy_pass http://localhost:${toString config.services.nix-serve.port};
        }
      '';
    };
  };
}
