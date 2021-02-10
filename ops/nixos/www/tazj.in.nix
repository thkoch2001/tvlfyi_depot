# serve tazjin's website & blog
{ config, lib, pkgs, ... }:

let depot = config.depot;
in {
  imports = [
    ./base.nix
  ];

  config = {
    services.nginx.virtualHosts."tazj.in" = {
      enableACME = true;
      forceSSL = true;
      root = depot.users.tazjin.homepage;

      extraConfig = ''
        ${depot.users.tazjin.blog.oldRedirects}
        location /blog/ {
          alias ${depot.users.tazjin.blog.rendered}/;

          if ($request_uri ~ ^/(.*)\.html$) {
            return 302 /$1;
          }

          try_files $uri $uri.html $uri/ =404;
        }
      '';
    };

    services.nginx.virtualHosts."git.tazj.in" = {
      enableACME = true;
      extraConfig = "return 301 https://code.tvl.fyi$request_uri;";
    };
  };
}
