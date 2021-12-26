{ config, lib, pkgs, depot, ... }:

let
  bbbg = depot.users.grfn.bbbg;
  cfg = config.services.bbbg;
in {
  options = with lib; {
    services.bbbg = {
      enable = mkEnableOption "BBBG Server";

      port = mkOption {
        type = types.int;
        default = 7222;
        description = "Port to listen to for the HTTP server";
      };

      domain = mkOption {
        type = types.str;
        default = "bbbg.gws.fyi";
        description = "Domain to host under";
      };

      proxy = {
        enable = mkEnableOption "NGINX reverse proxy";
      };

      database = {
        enable = mkEnableOption "BBBG Database Server";

        user = mkOption {
          type = types.str;
          default = "bbbg";
          description = "Database username";
        };

        host = mkOption {
          type = types.str;
          default = "localhost";
          description = "Database host";
        };

        name = mkOption {
          type = types.str;
          default = "bbbg";
          description = "Database name";
        };

        port = mkOption {
          type = types.int;
          default = 5432;
          description = "Database host";
        };
      };
    };
  };

  config = lib.mkMerge [
    (lib.mkIf cfg.enable {
      systemd.services.bbbg-server = {
        wantedBy = [ "multi-user.target" ];
        after = [ "network.target" ];

        serviceConfig = {
          DynamicUser = true;
          Restart = "always";
          EnvironmentFile = "/run/agenix/bbbg";
        };

        environment = {
          PGHOST = cfg.database.host;
          PGUSER = cfg.database.user;
          PGDATABASE = cfg.database.name;
          PORT = toString cfg.port;
        };

        script = "${bbbg.server}/bin/bbbg-server";
      };

      systemd.services.migrate-bbbg = {
        description = "Run database migrations for BBBG";
        wantedBy = [ "bbbg-server.service" ];
        after = ([ "network.target" ]
                 ++ (if cfg.database.enable
                     then ["postgresql.service"]
                     else []));

        serviceConfig = {
          Type = "oneshot";
          EnvironmentFile = "/run/agenix/bbbg";
        };

        environment = {
          PGHOST = cfg.database.host;
          PGUSER = cfg.database.user;
          PGDATABASE = cfg.database.name;
        };

        script = "${bbbg.db-util}/bin/bbbg-db-util migrate";
      };
    })
    (lib.mkIf cfg.database.enable {
      services.postgresql = {
        enable = true;
        authentication = lib.mkForce ''
          local all all trust
          host all all 127.0.0.1/32 password
          host all all ::1/128 password
          hostnossl all all 127.0.0.1/32 password
          hostnossl all all ::1/128  password
        '';

        ensureDatabases = [
          cfg.database.name
        ];

        ensureUsers = [{
          name = cfg.database.user;
          ensurePermissions = {
            "DATABASE ${cfg.database.name}" = "ALL PRIVILEGES";
          };
        }];
      };
    })
    (lib.mkIf cfg.proxy.enable {
      services.nginx = {
        enable = true;
        virtualHosts."${cfg.domain}" = {
          enableACME = true;
          forceSSL = true;
          locations."/".proxyPass = "http://localhost:${toString cfg.port}";
        };
      };
    })
  ];
}
