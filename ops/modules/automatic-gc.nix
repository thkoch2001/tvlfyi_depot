# Defines a service for automatically collecting Nix garbage
# periodically, without relying on the (ostensibly broken) Nix options
# for min/max space available.
{ config, lib, pkgs, ... }:

let
  cfg = config.services.depot.automatic-gc;
  description = "Automatically collect Nix garbage";

  GiBtoKiB = n: n * 1024 * 1024;
  GiBtoBytes = n: n * 1024 * 1024 * 1024;

  gcScript = pkgs.writeShellScript "automatic-nix-gc" ''
    set -ueo pipefail

    readonly MIN_THRESHOLD_KIB="''${1}"
    readonly MAX_FREED_BYTES="''${2}"
    readonly GEN_THRESHOLD="''${3}"
    readonly AVAILABLE_KIB=$(df --sync /nix --output=avail | tail -n1)

    if [ "''${AVAILABLE_KIB}" -lt "''${MIN_THRESHOLD_KIB}" ]; then
      echo "Have ''${AVAILABLE_KIB} KiB, but want ''${MIN_THRESHOLD_KIB} KiB."
      echo "Triggering Nix garbage collection up to ''${MAX_FREED_BYTES} bytes."
      set -x
      nix-collect-garbage \
        --delete-older-than "''${GEN_THRESHOLD}" \
        --max-freed "''${MAX_FREED_BYTES}"
    else
      echo "Skipping GC, enough space available"
    fi
  '';
in {
  options.services.depot.automatic-gc = {
    enable = lib.mkEnableOption description;

    interval = lib.mkOption {
      type = lib.types.string;
      example = "1h";
      description = ''
        Interval between garbage collection runs, specified in
        systemd.time(7) format.
      '';
    };

    diskThreshold = lib.mkOption {
      type = lib.types.int;
      example = "100";
      description = ''
        Minimum amount of space that needs to be available (in GiB) on
        the partition holding /nix. Garbage collection is triggered if
        it falls below this.
      '';
    };

    maxFreed = lib.mkOption {
      type = lib.types.int;
      example = "420";
      description = ''
        Maximum amount of space to free in a single GC run, in GiB.
      '';
    };

    preserveGenerations = lib.mkOption {
      type = lib.types.string;
      default = "90d";
      description = ''
        Preserve NixOS generations younger than the specified value,
        in the format expected by nix-collect-garbage(1).
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.services.automatic-gc = {
      inherit description;
      script = "${gcScript} ${toString (GiBtoKiB cfg.diskThreshold)} ${toString (GiBtoBytes cfg.maxFreed)} ${cfg.preserveGenerations}";
      serviceConfig.Type = "oneshot";
    };

    systemd.timers.automatic-gc = {
      inherit description;
      wantedBy = [ "multi-user.target" ];

      timerConfig = {
        OnActiveSec = "1";
        OnUnitActiveSec = cfg.interval;
      };
    };
  };
}
