{
  depot,
  pkgs,
  lib,
  ...
}:

let
  bins = depot.nix.getBins pkgs.findutils [ "find" ];
in
depot.nix.readTree.drvTargets {

  findia =
    depot.nix.writeExecline "findia"
      {
        readNArgs = 1;
        # TODO: comment out, thanks to sterni blocking the runExecline change
        # meta.description = ''
        #   Find case-insensitive anywhere (globbing)

        #   Usage: findia <pattern> <more find(1) arguments>
        # '';
      }
      [
        bins.find
        "-iname"
        "*\${1}*"
        "$@"
      ];

  findial =
    depot.nix.writeExecline "findial"
      {
        readNArgs = 1;
        # TODO: comment out, thanks to sterni blocking the runExecline change
        # meta.description = ''
        #   Find case-insensitive anywhere (globbing), follow symlinks";

        #   Usage: findial <pattern> <more find(1) arguments>
        # '';
      }
      [
        bins.find
        "-L"
        "-iname"
        "*\${1}*"
        "$@"
      ];

  findian =
    depot.nix.writeExecline "findian"
      {
        readNArgs = 2;
        # TODO: comment out, thanks to sterni blocking the runExecline change
        # meta.description = ''
        #   Find case-insensitive anywhere (globbing) in directory

        #   Usage: findian <directory> <pattern> <more find(1) arguments>
        # '';
      }
      [
        bins.find
        "$1"
        "-iname"
        "*\${2}*"
        "$@"
      ];

  findiap =
    depot.nix.writeExecline "findiap"
      {
        readNArgs = 2;
        # TODO: comment out, thanks to sterni blocking the runExecline change
        # meta.description = ''
        #   Find case-insensitive anywhere (globbing) in directory, the pattern allows for paths.

        #   Usage: findiap <directory> <pattern> <more find(1) arguments>
        # '';
      }
      [
        bins.find
        "$1"
        "-ipath"
        "*\${2}*"
        "$@"
      ];

  bell = depot.nix.writeExecline "bell" { } [
    "if"
    [
      "pactl"
      "upload-sample"
      "${pkgs.sound-theme-freedesktop}/share/sounds/freedesktop/stereo/complete.oga"
      "bell-window-system"
    ]
    "pactl"
    "play-sample"
    "bell-window-system"
  ];
}
