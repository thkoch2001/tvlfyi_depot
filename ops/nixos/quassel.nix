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
      type = lib.types.str;
      description = "ACME host to use for the Quassel TLS certificate";
    };

    interfaces = mkOption {
      default = [ "127.0.0.1" ];
      description = "The interfaces Quassel will be listening on.";
    };

    port = mkOption {
      default = 6698;
      description = ''
        The port number the Quassel daemon will be listening to.
      '';
    };
  };

  config = with lib; mkIf cfg.enable {
    systemd.services.quassel = {
      description = "Quassel IRC daemon";
      wantedBy = [ "multi-user.target" ];

      script = concatStringsSep " " [
        "${quasselDaemon}/bin/quasselcore"
        "--listen=${concatStringsSep "," cfg.interfaces}"
        "--port=${toString cfg.port}"
        "--configdir=/var/lib/quassel"
        "--require-ssl"
        "--ssl-cert=/var/lib/acme/${cfg.acmeHost}/full.pem"
      ];

      serviceConfig = {
        Restart = "always";
        DynamicUser = true;
        StateDirectory = "quassel";
        SupplementaryGroups = "acme";
      };
    };
  };
}
