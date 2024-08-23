{ depot, config, pkgs, lib, ... }:

let
  cfg = config.services.depot.cheddar;
  description = "cheddar - markdown/highlighting server";
in
{
  options.services.depot.cheddar = with lib; {
    enable = mkEnableOption description;
    port = mkOption {
      description = "Port on which cheddar should listen";
      type = types.int;
      default = 4238;
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.services.cheddar-server = {
      inherit description;
      wantedBy = [ "multi-user.target" ];
      script = "${depot.tools.cheddar}/bin/cheddar --listen 0.0.0.0:${toString cfg.cheddarPort} --sourcegraph-server";

      serviceConfig = {
        DynamicUser = true;
        Restart = "always";
      };
    };
  };
}
