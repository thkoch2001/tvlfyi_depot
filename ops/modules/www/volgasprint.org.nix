{ depot, ... }:

{
  imports = [
    ./base.nix
  ];

  config = {
    services.nginx.virtualHosts."volgasprint.org" = {
      enableACME = true;
      forceSSL = true;
      root = "${depot.web.volgasprint}";
    };
  };
}
