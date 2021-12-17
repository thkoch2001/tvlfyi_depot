# Defines a service for automatically and periodically calling depot's
# rebuild-system on a NixOS machine.
{ depot, config, lib, pkgs, ... }:

let
  cfg = config.services.depot.auto-deploy;
  description = "to automatically rebuild the current system's NixOS config from the latest checkout of depot";

  rebuild-system = depot.ops.nixos.rebuildSystemWith "$STATE_DIRECTORY/deploy";
  deployScript = pkgs.writeShellScript "auto-deploy" ''
    set -ueo pipefail

    if [[ $EUID -ne 0 ]]; then
      echo "Oh no! Only root is allowed to run auto-deploy!" >&2
      exit 1
    fi

    readonly depot=$STATE_DIRECTORY/depot.git
    readonly deploy=$STATE_DIRECTORY/deploy
    readonly git="git -C $depot"

    # find-or-create depot
    if [ ! -d $depot ]; then
      # cannot use $git here because $depot doesn't exist
      git clone --bare ${cfg.git-remote} $depot
    fi

    function cleanup() {
      $git worktree remove $deploy
    }
    trap cleanup EXIT

    $git fetch origin
    $git worktree add --force $deploy
    ${rebuild-system}/bin/rebuild-system
    $git worktree remove $deploy
  '';
in {
  options.services.depot.auto-deploy = {
    enable = lib.mkEnableOption description;

    git-remote = lib.mkOption {
      type = lib.types.str;
      default = "https://cl.tvl.fyi/depot.git";
      description = ''
        The (possibly remote) repository from which to clone as specified by the
        GIT URLS section of `man git-clone`.
      '';
    };

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
      path = [
        pkgs.bash
        pkgs.git
      ];

      # We need to prevent NixOS from interrupting us while it attempts to
      # restart systemd units.
      restartIfChanged = false;
      stopIfChanged = false;

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
