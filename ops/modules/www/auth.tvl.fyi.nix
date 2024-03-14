{ config, ... }:

{
  imports = [ ./base.nix ];

  config = {
    services.nginx.virtualHosts."auth.tvl.fyi" = {
      serverName = "auth.tvl.fyi";
      enableACME = true;
      forceSSL = true;

      extraConfig = ''
        # increase buffer size for large headers
        proxy_buffers 8 16k;
        proxy_buffer_size 16k;

        location / {
          proxy_pass http://localhost:${toString config.services.keycloak.settings.http-port};
          proxy_set_header X-Forwarded-For $remote_addr;
          proxy_set_header X-Forwarded-Proto https;
          proxy_set_header Host $host;
        }
      '';
    };
  };
}
