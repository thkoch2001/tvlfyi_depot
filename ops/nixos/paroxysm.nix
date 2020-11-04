{ config, lib, pkgs, ... }:

let
  cfg = config.services.depot.paroxysm;
  description = "TVL's majestic IRC bot";
in {
  options.services.depot.paroxysm.enable = lib.mkEnableOption description;

  config = lib.mkIf cfg.enable {
    systemd.services.paroxysm = {
      inherit description;
      script = "${config.depot.fun.paroxysm}/bin/paroxysm";
      wantedBy = [ "multi-user.target" ];

      environment = {
        PARX_DATABASE_URL = "postgresql://tvldb:tvldb@localhost/tvldb";
        PARX_IRC_CONFIG_PATH = "/var/lib/paroxysm/irc.toml";
      };

      serviceConfig = {
        DynamicUser = true;
        StateDirectory = "paroxysm";
        Restart = "always";
      };
    };
  };
}
