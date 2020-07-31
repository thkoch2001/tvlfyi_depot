{ config, lib, pkgs, ... }:

let
  cfg = config.services.depot.paroxysm;
  description = "TVL's majestic IRC bot";
in {
  options.services.depot.paroxysm.enable = mkEnableOption description;

  config = lib.mkIf cfg.enable {
    systemd.services.paroxysm = {
      inherit description;

      environment = {
        PARX_DATABASE_URL = "postgresql://tvldb:tvldb@localhost/tvldb";
        PARX_IRC_CONFIG_PATH = "/etc/secrets/tvlbot.conf";
      };
    };
  };
}
