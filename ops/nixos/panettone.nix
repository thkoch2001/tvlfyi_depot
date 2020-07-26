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

    dbName = mkOption {
      description = "Name of the database for Panettone";
      type = types.string;
      default = "panettone";
    };

    dbUser = mkOption {
      description = "Name of the database user for Panettone";
      type = types.string;
      default = "panettone";
    };
  };

  config = lib.mkIf cfg.enable {
    services.postgresql = {
      enable = true;
      enableTCPIP = true;

      authentication = lib.mkOverride 10 ''
        local all all trust
        host all all ::1/128 trust
      '';

      ensureDatabases = [ cfg.dbName ];

      ensureUsers = [{
        name = cfg.dbUser;
        ensurePermissions = {
          "DATABASE ${cfg.dbName}" = "ALL PRIVILEGES";
        };
      }];
    };

    systemd.services.panettone = {
      wantedBy = [ "multi-user.target" ];
      script = "${depot.web.panettone}/bin/panettone";

      serviceConfig = {
        DynamicUser = true;
        Restart = "always";
        StateDirectory = "panettone";
      };

      environment = {
        PANETTONE_PORT = toString cfg.port;
        PANETTONE_DATA_DIR = "/var/lib/panettone";
        PGHOST = "localhost";
        PGUSER = cfg.dbUser;
        PGDATABASE = cfg.dbName;
      };
    };
  };
}
