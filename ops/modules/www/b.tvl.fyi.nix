{ config, ... }:

{
  imports = [ ./base.nix ];

  config = {
    services.nginx.virtualHosts."b-shortlink" = {
      serverName = "b";
      extraConfig = "return 302 https://b.tvl.fyi$request_uri;";
    };

    services.nginx.virtualHosts."b.tvl.fyi" = {
      serverName = "b.tvl.fyi";
      serverAliases = [ "b.tvl.su" ];
      enableACME = true;
      forceSSL = true;

      extraConfig = ''
        # Forward short links to issues to the issue itself (b/32)
        location ~ ^/(\d+)$ {
          return 302 https://b.tvl.fyi/issues$request_uri;
        }

        location / {
          proxy_pass http://localhost:${
            toString config.services.depot.panettone.port
          };
        }
      '';
    };
  };
}
