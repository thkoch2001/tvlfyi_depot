{ pkgs, depot, ... }:

{
  config = {
    services.nginx.virtualHosts."billandhiscomputer.com" = {
      enableACME = true;
      forceSSL = true;
      root = depot.users.wpcarro.website.root;
    };
  };
}
