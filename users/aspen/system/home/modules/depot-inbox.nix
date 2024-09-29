{ config, lib, pkgs, ... }:

let
  depot = config.lib.depot;
in

{
  systemd.user = {
    services.sync-depot-public-inbox = {
      Service.ExecStart = pkgs.writeShellScript "sync-depot-public-inbox" ''
      ${depot.tools.fetch-depot-inbox}/bin/fetch-depot-inbox \
        /home/aspen/mail/tvl/
      ${pkgs.notmuch}/bin/notmuch new
    '';
    };

    timers.sync-depot-public-inbox = {
      Unit.Description = "Sync the depot public inbox";
      Timer = {
        OnCalendar = "*:*";
        Unit = "sync-depot-public-inbox.service";
      };
      Install.WantedBy = [ "timers.target" ];
    };
  };
}
