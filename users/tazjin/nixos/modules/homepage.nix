# serve tazjin's website & blog
{ depot, config, lib, pkgs, ... }:

{
  config = {
    services.nginx.virtualHosts."tazj.in" = {
      enableACME = true;
      forceSSL = true;
      root = depot.users.tazjin.homepage;
      serverAliases = [ "www.tazj.in" ];

      extraConfig = ''
        location = /en/rss.xml {
          return 301 https://tazj.in/feed.atom;
        }

        ${depot.users.tazjin.blog.oldRedirects}
        location /blog/ {
          alias ${depot.users.tazjin.blog.rendered}/;

          if ($request_uri ~ ^/(.*)\.html$) {
            return 302 /$1;
          }

          try_files $uri $uri.html $uri/ =404;
        }

        location = /predlozhnik {
          return 302 https://predlozhnik.ru;
        }

        # redirect for easier entry on a TV
        location = /tv {
          return 302 https://tazj.in/blobs/play.html;
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
