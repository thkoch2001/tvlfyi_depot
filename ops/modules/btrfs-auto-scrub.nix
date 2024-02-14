# Automatically performs a scrub on all btrfs filesystems configured in
# `config.fileSystems` on a daily schedule (by default). Activated by importing.
{ config, lib, ... }:

{
  config = {
    services = {
      btrfs.autoScrub = {
        enable = true;
        interval = lib.mkDefault "*-*-* 03:30:00";
        # gather all btrfs fileSystems, extra ones can be added via the NixOS
        # module merging mechanism, of course.
        fileSystems = lib.concatLists (
          lib.mapAttrsToList
            (
              _:
              { fsType, mountPoint, ... }:
              if fsType == "btrfs" then [ mountPoint ] else [ ]
            )
            config.fileSystems
        );
      };
    };
  };
}
