# Defines a service for automatically and periodically calling depot's
# rebuild-system on a NixOS machine.
{ config, lib, pkgs, ... }:

let
  cfg = config.services.depot.auto-deploy;
  description = "to automatically rebuild the current system's NixOS config from the latest checkout of depot";

  deployScript = pkgs.writeShellScript "auto-deploy" ''
    set -ueo pipefail

    if [[ $EUID -ne 0 ]]; then
      echo "Oh no! Only root is allowed to run auto-deploy!" >&2
      exit 1
    fi

    readonly depot=$STATE_DIRECTORY/depot.git
    readonly deploy=$STATE_DIRECTORY/deploy
    readonly git="${pkgs.git}/bin/git -C $depot"

    # find-or-create depot
    if [ ! -d $depot ]; then
      # cannot use $git here because $depot doesn't exist
      ${pkgs.git}/bin/git clone --bare https://cl.tvl.fyi/depot.git $depot
    fi

    # clean-up in case things go sideways
    trap "$git worktree remove $deploy" EXIT

    $git fetch origin
    $git worktree add $deploy
    $deploy/bin/rebuild-system
    $git worktree remove $deploy
  '';
in {
  options.services.depot.auto-deploy = {
    enable = lib.mkEnableOption description;

    interval = lib.mkOption {
      type = lib.types.str;
      example = "1h";
      description = ''
        Interval between Nix builds, specified in systemd.time(7) format.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.services.auto-deploy = {
      inherit description;
      script = "${deployScript}";

      serviceConfig = {
        Type = "oneshot";
        StateDirectory = "auto-deploy";
      };
    };

    systemd.timers.auto-deploy = {
      inherit description;
      wantedBy = [ "multi-user.target" ];

      timerConfig = {
        OnActiveSec = "1";
        OnUnitActiveSec = cfg.interval;
      };
    };
  };
}
