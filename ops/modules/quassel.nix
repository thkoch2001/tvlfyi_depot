# A more modern module for running Quassel.
{ config, lib, pkgs, ... }:

let
  cfg = config.services.depot.quassel;
  quasselDaemon = pkgs.quassel.override {
    monolithic = false;
    enableDaemon = true;
    withKDE = false;
  };
in {
  options.services.depot.quassel = with lib; {
    enable = mkEnableOption "Quassel IRC daemon";

    acmeHost = mkOption {
      description = "ACME host to use for the Quassel TLS certificate";
      type = lib.types.str;
    };

    bindAddresses = mkOption {
      description = "Addresses Quassel will bind to/listen on";
      default = [ "127.0.0.1" ];
    };

    logLevel = mkOption {
      description = "Log level for Quassel Core";
      default = "Info";
      type = lib.types.enum [ "Debug" "Info" "Warning" "Error" ];
    };

    port = mkOption {
      default = 6698;
      description = ''
        The port number the Quassel daemon will be listening to.
      '';
    };
  };

  config = with lib;
    mkIf cfg.enable {
      systemd.services.quassel = {
        description = "Quassel IRC daemon";
        wantedBy = [ "multi-user.target" ];

        script = concatStringsSep " " [
          "${quasselDaemon}/bin/quasselcore"
          "--listen=${concatStringsSep "," cfg.bindAddresses}"
          "--port=${toString cfg.port}"
          "--configdir=/var/lib/quassel"
          "--require-ssl"
          "--ssl-cert=/var/lib/acme/${cfg.acmeHost}/full.pem"
          "--loglevel=${cfg.logLevel}"
        ];

        serviceConfig = {
          Restart = "always";
          User = "quassel";
          Group = "quassel";
          StateDirectory = "quassel";
        };
      };

      users = {
        users.quassel = {
          isSystemUser = true;
          group = "quassel";
        };

        groups.quassel = { };
      };
    };
}
