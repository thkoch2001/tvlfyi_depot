{ depot, ... }:

{
  imports = [ ./base.nix ];

  config = {
    services.nginx.virtualHosts."todo.tvl.fyi" = {
      serverName = "todo.tvl.fyi";
      serverAliases = [ "todo.tvl.su" ];
      root = depot.web.todolist;
      enableACME = true;
      forceSSL = true;

      extraConfig = ''
        add_header Strict-Transport-Security "max-age=31536000; includeSubDomains; preload" always;

        location ~* \.(webp|woff2)$ {
          add_header Cache-Control "public, max-age=31536000";
        }
      '';
    };
  };
}
