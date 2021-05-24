{ depot, lib, pkgs, ... }:

{
  imports = [
    ./base.nix
  ];

  config = {
    services.nginx.virtualHosts."wigglydonke.rs" = {
      enableACME = true;
      forceSSL = true;
      root = "${depot.path + "/users/grfn/wigglydonke.rs"}";
    };
  };
}
