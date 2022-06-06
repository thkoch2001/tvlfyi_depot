{ depot, ... }:

{
  config = {
    services.spacecookie = {
      enable = true;
      openFirewall = true;
      settings = {
        hostname = "sterni.lv";
        root = depot.users.sterni.lv.gopher;
        log = {
          enable = true;
          hide-ips = true;
          hide-time = true;
        };
      };
    };
  };
}
