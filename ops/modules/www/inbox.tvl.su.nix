{ config, depot, ... }:

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
          alias ${depot.web.inbox};
        }

        location / {
          proxy_pass http://localhost:${toString config.services.public-inbox.http.port};
        }
      '';
    };
  };
}
