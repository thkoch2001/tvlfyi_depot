# Configuration for running the TVL cgit instance using thttpd.
{ config, depot, lib, pkgs, ... }:

let
  cfg = config.services.depot.cgit;
in
{
  options.services.depot.cgit = with lib; {
    enable = mkEnableOption "Run cgit web interface for depot";

    port = mkOption {
      description = "Port on which cgit should listen";
      type = types.int;
      default = 2448;
    };

    repo = mkOption {
      description = "Path to depot's .git folder on the machine";
      type = types.str;
      default = "/var/lib/gerrit/git/depot.git/";
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.services.cgit = {
      wantedBy = [ "multi-user.target" ];

      serviceConfig = {
        Restart = "on-failure";
        User = "git";
        Group = "git";

        ExecStart = depot.web.cgit-tvl.override {
          inherit (cfg) port repo;
        };
      };
    };
  };
}
