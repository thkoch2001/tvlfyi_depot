{ config, lib, pkgs, ... }:

let
  cfg = config.services.depot.panettone;
  depot = config.depot;
in {
  options.services.depot.panettone = with lib; {
    enable = mkEnableOption "Panettone issue tracker";

    port = mkOption {
      description = "Port on which Panettone should listen";
      type = types.int;
      default = 7268;
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.services.panettone = {
      wantedBy = [ "multi-user.target" ];
      script = "${depot.web.panettone}/bin/panettone";

      serviceConfig = {
        DynamicUser = true;
        Restart = "always";
        StateDirectory = "panettone";
      };

      environment = {
        PANETTONE_PORT = cfg.port;
        PANETTONE_DATA_DIR = "/var/lib/panettone";
      };
    };
  };
}
