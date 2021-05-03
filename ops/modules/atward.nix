{ depot, config, lib, pkgs, ... }:

let
  cfg = config.services.depot.atward;
  description = "atward - (attempt to) cleverly route queries";
in {
  options.services.depot.atward = {
    enable = lib.mkEnableOption description;

    listenAddress = lib.mkOption {
      type = lib.types.str;
      default = "[::1]:28973";
      description = "Address on which atward should listen";
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

      environment.ATWARD_LISTEN_ADDRESS = toString cfg.listenAddress;
    };
  };
}
