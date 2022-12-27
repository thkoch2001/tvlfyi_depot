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
        # nginx is incapable of serving a single file at /, hence this hack:
        location = / {
          index /landing-page;
        }

        location = /landing-page {
          alias ${depot.web.inbox};
        }

        # rest of requests is proxied to public-inbox-httpd
        location / {
          proxy_pass http://localhost:${toString config.services.public-inbox.http.port};
        }
      '';
    };
  };
}
