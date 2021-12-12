{ depot, config, lib, pkgs, ... }:

let cfg = config.services.depot.panettone;
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
      type = types.str;
      default = "localhost";
    };

    dbName = mkOption {
      description = "Name of the database for Panettone";
      type = types.str;
      default = "panettone";
    };

    dbUser = mkOption {
      description = "Name of the database user for Panettone";
      type = types.str;
      default = "panettone";
    };

    secretsFile = mkOption {
      description = ''
        Path to a file containing secrets, in the format accepted
        by systemd's EnvironmentFile
      '';
      type = types.str;
      default = "/run/agenix/panettone";
    };

    irccatHost = mkOption {
      description = "Hostname for the irccat instance";
      type = types.str;
      default = "localhost";
    };

    irccatPort = mkOption {
      description = "Port for the irccat instance";
      type = types.int;
      default = 4722;
    };

    irccatChannel = mkOption {
      description = "IRC channels to post to via irccat";
      type = types.str;
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = cfg.dbHost != "localhost"
          || config.services.postgresql.enable;
        message = "Panettone requires a postgresql database";
      }
      {
        assertion = cfg.dbHost != "localhost"
          || config.services.postgresql.enableTCPIP;
        message =
          "Panettone can only connect to the postgresql database over TCP";
      }
      {
        assertion = cfg.dbHost != "localhost"
          || (lib.any (user: user.name == cfg.dbUser)
            config.services.postgresql.ensureUsers);
        message = "Panettone requires a database user";
      }
      {
        assertion = cfg.dbHost != "localhost" || (lib.any (db: db == cfg.dbName)
          config.services.postgresql.ensureDatabases);
        message = "Panettone requires a database";
      }
    ];

    systemd.services.panettone = {
      wantedBy = [ "multi-user.target" ];
      script = "${depot.web.panettone}/bin/panettone";

      serviceConfig = {
        DynamicUser = true;
        Restart = "always";
        EnvironmentFile = cfg.secretsFile;
      };

      environment = {
        PANETTONE_PORT = toString cfg.port;
        PGHOST = "localhost";
        PGUSER = cfg.dbUser;
        PGDATABASE = cfg.dbName;
        IRCCATHOST = cfg.irccatHost;
        IRCCATPORT = toString cfg.irccatPort;
        ISSUECHANNEL = cfg.irccatChannel;
      };
    };
  };
}
