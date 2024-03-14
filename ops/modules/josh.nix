# Configures the public josh instance for serving the depot.
{
  config,
  depot,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.services.depot.josh;
in
{
  options.services.depot.josh = with lib; {
    enable = mkEnableOption "Enable josh for serving the depot";

    port = mkOption {
      description = "Port on which josh should listen";
      type = types.int;
      default = 5674;
    };
  };

  config = lib.mkIf cfg.enable {
    # Run josh for the depot.
    systemd.services.josh = {
      description = "josh - partial cloning of monorepos";
      wantedBy = [ "multi-user.target" ];
      path = [
        pkgs.git
        pkgs.bash
      ];

      serviceConfig = {
        DynamicUser = true;
        StateDirectory = "josh";
        Restart = "always";
        ExecStart = "${depot.third_party.josh}/bin/josh-proxy --no-background --local /var/lib/josh --port ${toString cfg.port} --remote https://cl.tvl.fyi/ --require-auth";
      };
    };
  };
}
