# Configure restic backups to Google Cloud Storage.
#
# Conventions: Restic state is kept in /var/backup/restic, with
# secrets in /var/backup/restic/secret.
{ config, lib, pkgs, ... }:

let
  cfg = config.services.depot.restic;
  description = "Restic backups to GCS";
  mkStringOption = default: lib.mkOption {
    inherit default;
    type = lib.types.string;
  };
in {
  options.services.depot.restic = {
    enable = lib.mkEnableOption description;
    project = mkStringOption "tazjins-infrastructure";
    credentialPath = mkStringOption "/var/backup/restic/gcp-key.json";
    repository = mkStringOption "gs:tvl-fyi-backups:/whitby";
    interval = mkStringOption "hourly";

    paths = with lib; mkOption {
      description = "Directories that should be backed up";
      type = types.listOf types.string;
    };

    exclude = with lib; mkOption {
      description = "Files that should be excluded from backups";
      type = types.listOf types.string;
    };
  };

  config = lib.mkIf cfg.enable {
    # Regularly back up whitby to Google Cloud Storage.
    systemd.services.restic = {
      description = "Backups to Google Cloud Storage";

      script = "${pkgs.restic}/bin/restic backup ${lib.concatStringsSep " " cfg.paths}";

      environment = {
        GOOGLE_PROJECT_ID = cfg.project;
        GOOGLE_APPLICATION_CREDENTIALS = cfg.credentialPath;
        RESTIC_REPOSITORY = cfg.repository;
        RESTIC_PASSWORD_FILE = "/var/backup/restic/secret";
        RESTIC_CACHE_DIR = "/var/backup/restic/cache";

        RESTIC_EXCLUDE_FILE =
          builtins.toFile "exclude-files" (lib.concatStringsSep "\n" cfg.exclude);
      };
    };

    systemd.timers.restic = {
      wantedBy = [ "multi-user.target" ];
      timerConfig.OnCalendar = cfg.interval;
    };
  };
}
