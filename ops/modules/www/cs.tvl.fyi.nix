# This configuration redirects from the previous Sourcegraph instance to
# livegrep/cgit where appropriate.
{ config, ... }:

{
  imports = [
    ./base.nix
  ];

  config = {
    services.nginx.virtualHosts."cs.tvl.fyi" = {
      serverName = "cs.tvl.fyi";
      serverAliases = [ "cs.tvl.su" ];
      enableACME = true;
      forceSSL = true;

      extraConfig = ''
        set $lineno "";

        # depot root
        location = /depot {
            return 301 https://code.tvl.fyi/tree/;
        }

        # folder/file on canon
        location ~ ^/depot/-/(blob|tree)/(.*)$ {
            set $path $2;
            if ($args ~ ^L(\d+)(-\d+)?$) {
                set $lineno "#n$1";
            }

            return 302 https://code.tvl.fyi/tree/$path$lineno;
        }

        # folder/file on specific commit
        location ~ ^/depot@([a-f0-9]+)/-/(blob|tree)/(.*)$ {
            set $commit $1;
            set $path $3;

            if ($args ~ ^L(\d+)(-\d+)?$) {
                set $lineno "#n$1";
            }

            return 302 https://code.tvl.fyi/tree/$path?id=$commit$lineno;
        }

        # commit info
        location ~ ^/depot/-/commit/(.*)$ {
            set $commit $1;
            return 302 https://code.tvl.fyi/commit/?id=$commit;
        }

        # search handler
        # This only redirects to the new search, it doesn't try to parse and
        # rewrite the query.
        location /search {
            return 302 https://grep.tvl.fyi/search;
        }

        location / {
            return 404 "TVL code search has moved to grep.tvl.fyi and we could not figure out how to rewrite your query. Sorry!";
        }
      '';
    };
  };
}
