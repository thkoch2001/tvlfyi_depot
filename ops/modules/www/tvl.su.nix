{ depot, ... }:

{
  imports = [
    ./base.nix
  ];

  config = {
    services.nginx.virtualHosts."tvl.su" = {
      serverName = "tvl.su";
      root = depot.corp.website;
      enableACME = true;
      forceSSL = true;

      extraConfig = ''
        location /ru {
          index index-ru.html;
        }

        location /en {
          index index-en.html;
        }

        add_header Strict-Transport-Security "max-age=31536000; includeSubDomains; preload" always;
      '';
    };
  };
}
