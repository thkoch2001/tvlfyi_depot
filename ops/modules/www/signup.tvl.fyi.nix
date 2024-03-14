{ depot, ... }:

{
  imports = [ ./base.nix ];

  config = {
    services.nginx.virtualHosts."signup.tvl.fyi" = {
      root = depot.web.pwcrypt;
      enableACME = true;
      forceSSL = true;

      extraConfig = ''
        add_header Strict-Transport-Security "max-age=31536000; includeSubDomains; preload" always;
      '';
    };
  };
}
