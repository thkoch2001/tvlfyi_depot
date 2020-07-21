{ config, lib, pkgs, ... }:

let
  cfg = config.services.depot.pannetone;
  depot = config.depot;
in {
  options.services.depot.pannetone = with lib; {
    enable = mkEnableOption "Pannetone issue tracker";

    port = mkOption {
      description = "Port on which Pannetone should listen";
      type = types.int;
      default = 7268;
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.services.pannetone = {
      wantedBy = [ "multi-user.target" ];
      script = "${depot.web.pannetone}/bin/pannetone";

      serviceConfig = {
        DynamicUser = true;
        Restart = "always";
        StateDirectory = "pannetone";
      };

      environment = {
        PANNETONE_PORT = cfg.port;
        PANNETONE_DATA_DIR = "/var/lib/pannetone";
      };
    };
  };
}
