# Host the static assets at static.tvl.fyi
#
# All assets are served from $base/$drvhash/$file, but can also be
# included with `latest/` which will return a (non-permanent!)
# redirect to the real location.
#
# For all purposes within depot, using the drvhash of web.static is
# recommended.
{ depot, pkgs, ... }:

let staticHash = depot.web.static.drvHash;
in {
  imports = [
    ./base.nix
  ];

  config = {
    services.nginx.virtualHosts."static.tvl.fyi" = {
      serverAliases = [ "static.tvl.su" ];
      enableACME = true;
      forceSSL = true;

      extraConfig = ''
        location /latest {
          rewrite ^/latest/(.*) /${staticHash}/$1 redirect;
        }

        location /${staticHash}/ {
          alias ${depot.web.static}/;
          expires max;
          add_header Cache-Control "public";
        }
      '';
    };
  };
}
