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

    dbHost = mkOption {
      description = "Postgresql host to connect to for Panettone";
      type = types.string;
      default = "localhost";
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
    systemd.services.panettone = {
      assertions = [{
        assertion =
          cfg.dbHost != "localhost" || config.services.postgresql.enable;
        message = "Panettone requires a postgresql database";
      } {
        assertion =
          cfg.dbHost != "localhost" || config.services.postgresql.enableTCPIP;
        message = "Panettone can only connect to the postgresql database over TCP";
      } {
        assertion =
          cfg.dbHost != "localhost" || (lib.any
            (user: user.name == cfg.dbUser)
            config.services.postgresql.ensureUsers);
        message = "Panettone requires a database user";
      } {
        assertion =
          cfg.dbHost != "localhost" || (lib.any
            (db: db == cfg.dbName)
            config.services.postgresql.ensureDatabases);
        message = "Panettone requires a database";
      }];

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
