# Configure restic backups to S3-compatible storage, in our case
# GleSYS object storage.
#
# Conventions:
# - restic's cache lives in /var/backup/restic/cache
# - repository password lives in /var/backup/restic/secret
# - object storage credentials in /var/backup/restic/glesys-key
{ config, lib, pkgs, ... }:

let
  cfg = config.services.depot.restic;
  description = "Restic backups to GleSYS";
  mkStringOption = default: lib.mkOption {
    inherit default;
    type = lib.types.str;
  };
in
{
  options.services.depot.restic = {
    enable = lib.mkEnableOption description;
    bucketEndpoint = mkStringOption "objects.dc-sto1.glesys.net";
    bucketName = mkStringOption "aged-resonance";
    bucketCredentials = mkStringOption "/var/backup/restic/glesys-key";
    repository = mkStringOption config.networking.hostName;
    interval = mkStringOption "hourly";

    paths = with lib; mkOption {
      description = "Directories that should be backed up";
      type = types.listOf types.str;
    };

    exclude = with lib; mkOption {
      description = "Files that should be excluded from backups";
      type = types.listOf types.str;
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.services.restic = {
      description = "Backups to GleSYS";

      script = "${pkgs.restic}/bin/restic backup ${lib.concatStringsSep " " cfg.paths}";

      environment = {
        RESTIC_REPOSITORY = "s3:${cfg.bucketEndpoint}/${cfg.bucketName}/${cfg.repository}";
        AWS_SHARED_CREDENTIALS_FILE = cfg.bucketCredentials;
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

    environment.systemPackages = [ pkgs.restic ];
  };
}
