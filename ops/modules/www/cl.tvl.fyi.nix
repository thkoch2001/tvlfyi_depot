{ config, ... }:

{
  imports = [ ./base.nix ];

  config = {
    services.nginx.virtualHosts."cl-shortlink" = {
      serverName = "cl";
      extraConfig = "return 302 https://cl.tvl.fyi$request_uri;";
    };

    services.nginx.virtualHosts.gerrit = {
      serverName = "cl.tvl.fyi";
      serverAliases = [ "cl.tvl.su" ];
      enableACME = true;
      forceSSL = true;

      extraConfig = ''
        location / {
          proxy_pass http://localhost:4778;
          proxy_set_header  X-Forwarded-For $remote_addr;
          # The :443 suffix is a workaround for https://b.tvl.fyi/issues/88.
          proxy_set_header  Host $host:443;
        }
      '';
    };
  };
}
