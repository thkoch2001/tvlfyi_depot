{ ... }:

{
  imports = [
    ./nginx.nix
  ];

  config = {
    services.nginx.virtualHosts."sterni.lv" = {
      enableACME = true;
      forceSSL = true;
      # TODO(sterni): take website from store, replace /tmp with a simple LRU thing
      root = toString /srv/http;
    };
  };
}
