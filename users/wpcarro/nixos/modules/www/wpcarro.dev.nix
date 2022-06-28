{ pkgs, ... }:

{
  config = {
    services.nginx.virtualHosts."wpcarro.dev" = {
      enableACME = true;
      forceSSL = true;
    };
  };
}
