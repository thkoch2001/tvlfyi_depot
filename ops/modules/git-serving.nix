# Configures public git-serving infrastructure for TVL, this involves:
#
# 1. cgit (running at code.tvl.fyi) for web views of the repository
# 2. josh (for cloning the repository and its distinct subtrees)
#
# We also run Sourcegraph for browsing the repository, but this is
# currently configured in a separate module
# (//ops/modules/sourcegraph.nix)
#
# TODO(tazjin): Move //web/cgit-taz configuration in here instead.
{ config, depot, lib, pkgs, ... }:

let cfg = config.services.depot.git-serving;
in {
  options.services.depot.git-serving = with lib; {
    enable = mkEnableOption "Enable cgit & josh configuration";

    joshPort = mkOption {
      description = "Port on which josh should listen";
      type = types.int;
      default = 5674;
    };
  };

  config = lib.mkIf cfg.enable {
    # Run cgit for the depot. The onion here is nginx(thttpd(cgit)).
    systemd.services.cgit = {
      wantedBy = [ "multi-user.target" ];
      script = "${depot.web.cgit-taz}/bin/cgit-launch";

      serviceConfig = {
        Restart = "on-failure";
        User = "git";
        Group = "git";
      };
    };

    # Run josh for the depot.
    systemd.services.josh = {
      description = "josh - partial cloning of monorepos";
      wantedBy = [ "multi-user.target" ];
      path = [ pkgs.git pkgs.bash ];

      serviceConfig = {
        DynamicUser = true;
        StateDirectory = "josh";
        Restart = "always";
        ExecStart =
          "${depot.third_party.josh}/bin/josh-proxy --no-background --local /var/lib/josh --port ${
            toString cfg.joshPort
          } --remote https://cl.tvl.fyi/";
      };
    };
  };
}
