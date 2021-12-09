# Configuration for the Gerrit autosubmit bot (//third_party/gerrit-queue)
{ depot, pkgs, config, lib, ... }:

let
  cfg = config.services.depot.gerrit-queue;
  description = "gerrit-queue - autosubmit bot for Gerrit";
  mkStringOption = default: lib.mkOption {
    inherit default;
    type = lib.types.str;
  };
in {
  options.services.depot.gerrit-queue = {
    enable = lib.mkEnableOption description;
    gerritUrl = mkStringOption "https://cl.tvl.fyi";
    gerritProject = mkStringOption "depot";
    gerritBranch = mkStringOption "canon";

    interval = with lib; mkOption {
      type = types.int;
      default = 60;
      description = "Interval (in seconds) for submit queue checks";
    };

    secretsFile = mkOption {
      description = "Path to a systemd EnvironmentFile containing secrets";
      default = "/run/agenix/gerrit-queue";
      type = types.str;
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.services.gerrit-queue = {
      inherit description;
      wantedBy = [ "multi-user.target" ];

      serviceConfig = {
        ExecStart = "${depot.third_party.gerrit-queue}/bin/gerrit-queue";
        DynamicUser = true;
        Restart = "always";
        EnvironmentFile = cfg.secretsFile;
      };

      environment = {
        GERRIT_URL = cfg.GerritUrl;
        GERRIT_PROJECT = cfg.gerritProject;
        GERRIT_BRANCH = cfg.gerritBranch;
        SUBMIT_QUEUE_TRIGGER_INTERVAL = toString cfg.interval;
      };
    };
  };
}
