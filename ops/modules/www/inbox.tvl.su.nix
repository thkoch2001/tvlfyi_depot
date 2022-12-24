{ config, ... }:

{
  imports = [
    ./base.nix
  ];

  config = {
    services.nginx.virtualHosts."inbox.tvl.su" = {
      enableACME = true;
      forceSSL = true;

      extraConfig = ''
        location = / {
          return 302 https://inbox.tvl.su/depot;
        }

        location / {
          proxy_pass http://localhost:${toString config.services.public-inbox.http.port};
        }
      '';
    };
  };
}
