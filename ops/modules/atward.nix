{ depot, config, lib, pkgs, ... }:

let
  cfg = config.services.depot.atward;
  description = "atward - (attempt to) cleverly route queries";
in {
  options.services.depot.atward = {
    enable = lib.mkEnableOption description;

    port = lib.mkOption {
      type = lib.types.int;
      default = 28973;
      description = "Port on which atward should listen";
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.services.atward = {
      inherit description;
      script = "${depot.web.atward}/bin/atward";
      wantedBy = [ "multi-user.target" ];

      serviceConfig = {
        DynamicUser = true;
        Restart = "always";
      };

      environment.ATWARD_PORT = toString cfg.port;
    };
  };
}
