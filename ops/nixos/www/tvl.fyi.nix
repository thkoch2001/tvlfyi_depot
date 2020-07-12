{ config, ... }:

{
  imports = [
    ./base.nix
  ];

  config = {
    services.nginx.virtualHosts."tvl.fyi" = {
      serverName = "tvl.fyi";
      root = config.depot.web.tvl;
      enableACME = true;
      forceSSL = true;

      extraConfig = ''
        add_header Strict-Transport-Security "max-age=31536000; includeSubDomains; preload" always;

        rewrite ^/builds/?$ https://buildkite.com/tvl/depot/ last;

        rewrite ^/monorepo-doc/?$ https://docs.google.com/document/d/1nnyByXcH0F6GOmEezNOUa2RFelpeRpDToBLYD_CtjWE/edit?usp=sharing last;

        rewrite ^/irc/?$ ircs://chat.freenode.net:6697/##tvl last;

        location ~* \.(webp|woff2)$ {
          add_header Cache-Control "public, max-age=31536000";
        }
      '';
    };
  };
}
