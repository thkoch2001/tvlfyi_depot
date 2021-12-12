# serve tazjin's website & blog
{ depot, config, lib, pkgs, ... }:

{
  imports = [ ./base.nix ];

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

        # Temporary place for serving static files.
        location /blobs/ {
          alias /var/lib/tazjins-blobs/;
        }
      '';
    };

    services.nginx.virtualHosts."git.tazj.in" = {
      enableACME = true;
      forceSSL = true;
      extraConfig = "return 301 https://code.tvl.fyi$request_uri;";
    };
  };
}
