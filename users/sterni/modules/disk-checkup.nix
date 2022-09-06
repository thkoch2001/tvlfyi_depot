# TODO(sterni): configure smartd and alerts
{ config, lib, ... }:

{
  config = {
    services = {
      btrfs.autoScrub = {
        enable = true;
        interval = "daily";
        # gather all btrfs fileSystems and overwrite default
        fileSystems = lib.mkForce (
          lib.concatLists (
            lib.mapAttrsToList
              (
                _:
                { fsType, mountPoint, ... }:
                if fsType == "btrfs" then [ mountPoint ] else [ ]
              )
              config.fileSystems
          )
        );
      };
    };
  };
}
