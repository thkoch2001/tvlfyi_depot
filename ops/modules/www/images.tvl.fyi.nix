{ config, ... }:

{
  imports = [ ./base.nix ];

  config = {
    services.nginx.virtualHosts."images.tvl.fyi" = {
      serverName = "images.tvl.fyi";
      serverAliases = [ "images.tvl.su" ];
      enableACME = true;
      forceSSL = true;

      extraConfig = ''
        location / {
          proxy_pass http://localhost:${
            toString config.services.depot.nixery.port
          };
        }
      '';
    };
  };
}
