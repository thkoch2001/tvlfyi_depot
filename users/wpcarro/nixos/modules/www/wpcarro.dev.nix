{ pkgs, ... }:

{
  config = {
    services.nginx.virtualHosts."wpcarro.dev" = {
      enableACME = true;
      forceSSL = true;
      extraConfig = "return 302 https://billandhiscomputer.com$request_uri;";
    };
  };
}
