{
  config,
  depot,
  lib,
  pkgs,
  ...
}:

{
  age.secrets.miniflux.file = depot.users.tazjin.secrets."miniflux.age";

  services.miniflux = {
    enable = true;
    adminCredentialsFile = "/run/agenix/miniflux";
    config.LISTEN_ADDR = "127.0.0.1:6359";
    config.BASE_URL = "https://feeds.tazj.in";
  };

  services.nginx.virtualHosts."feeds" = {
    serverName = "feeds.tazj.in";
    enableACME = true;
    forceSSL = true;

    locations."/" = {
      proxyPass = "http://127.0.0.1:6359";
    };
  };
}
