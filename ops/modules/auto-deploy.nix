# Defines a service for automatically and periodically calling depot's
# rebuild-system on a NixOS machine.
{ config, lib, pkgs, ... }:

let
  cfg = config.services.depot.auto-deploy;
  description = "Automatically build Nix things";

  deployScript = pkgs.writeShellScript "auto-deploy" ''
    set -ueo pipefail

    system=# ask binary cache for latest $HOSTNAME
    nix-env p /nix/var/nix/profiles/system --set $system
    $system/bin/switch-to-configuration switch
  '';
in {
  options.services.depot.auto-deploy = {
    enable = lib.mkEnableOption description;

    # TODO(wpcarro): This option is from my initial discussion with tazjin, but
    # now that I'm looking at it, I'm not sure what purpose it serves.
    target = lib.mkOption {
      type = lib.types.str;
      example = "//users/wpcarro/cucumber";
      description = ''
        Depot-pathspec to the target you'd like to build.
      '';
    };

    interval = lib.mkOption {
      type = lib.types.str;
      example = "1h";
      description = ''
        Interval between Nix builds, specified systemd.time(7) format.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.services.auto-deploy = {
      inherit description;
      script = "${deployScript}";
      # TODO(wpcarro): Ensure this value is correct
      serviceConfig.Type = "oneshot";
    };

    systemd.timers.auto-deploy = {
      inherit description;
      # TODO(wpcarro): Ensure that these two values are correct.
      requisite = [ "nix-daemon.service" ];
      wantedBy = [ "multi-user.target" ];

      timeConfig = {
        OnActiveSec = "1";
        OnUnitActiveSec = cfg.interval;
      };
    };
  };
}
