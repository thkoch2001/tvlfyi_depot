{ pkgs, ... }:

let
  arcStats = [
    ./zfs-arc-stats/0001-Specify-correct-MIB-length.patch
    ./zfs-arc-stats/0002-Support-ZFS-ARC-stats-on-FreeBSD.patch
    ./zfs-arc-stats/0003-ZFS-arcstats-for-Linux.patch
    ./zfs-arc-stats/0004-ZFS-arcstats-for-Darwin-macOS-OS-X.patch
    ./zfs-arc-stats/0005-Refactor-common-OpenZFS-sysctl-access.patch
    ./zfs-arc-stats/0006-ZFS-arcstats-for-Solaris.patch
    ./zfs-arc-stats/0007-Refactor-openzfs_sysctl_init-and-ZfsArcMeter.patch
    ./zfs-arc-stats/0008-Support-for-ZFS-Compressed-ARC-statistics.patch
    ./zfs-arc-stats/0009-Linux-fixes.patch
  ];

  arcCache = [
    ./zfs-arc-cache/0001-Linux-consider-ZFS-ARC-to-be-cache.patch
  ];

  removeMousing = [
    ./remove-mousing/0001-Tear-out-ncurses-mouse-mode.patch
  ];

  octoMeter = [
    ./octo-meter/0001-Add-quad-and-octo-meter-display-meters.patch
  ];
in
  with pkgs; htop.overrideAttrs
    ({ patches ? [], nativeBuildInputs ? [], ... }:
      {
        patches = patches ++ arcStats ++ arcCache ++ removeMousing ++ octoMeter;
        nativeBuildInputs = nativeBuildInputs ++ [ autoreconfHook ];
      })
