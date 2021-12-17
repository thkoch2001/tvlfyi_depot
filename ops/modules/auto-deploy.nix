# Defines a service for automatically and periodically calling depot's
# rebuild-system on a NixOS machine.
{ config, lib, pkgs, ... }:

let
  cfg = config.services.depot.auto-deploy;
  description = "Automatically build Nix things";

  deployScript = pkgs.writeShellScript "auto-deploy" ''
    set -ueo pipefail

    if [[ $EUID -ne 0 ]]; then
      echo "Oh no! Only root is allowed to run auto-deploy!" >&2
      exit 1
    fi

    readonly git=${pkgs.git}/bin/git

    # find-or-create depot
    if [ ! -d $STATE_DIRECTORY ]; then
      $git clone --bare https://cl.tvl.fyi/depot.git $STATE_DIRECTORY
    fi

    cd $STATE_DIRECTORY
    $git fetch origin
    $git worktree add ../deploy
    cd ../deploy
    ./bin/rebuild-system
    $git worktree remove ../deploy
  '';
in {
  options.services.depot.auto-deploy = {
    enable = lib.mkEnableOption description;

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

      serviceConfig = {
        Type = "oneshot";
        StateDirectory = "depot";
      };
    };

    systemd.timers.auto-deploy = {
      inherit description;
      wantedBy = [ "multi-user.target" ];

      timeConfig = {
        OnActiveSec = "1";
        OnUnitActiveSec = cfg.interval;
      };
    };
  };
}
