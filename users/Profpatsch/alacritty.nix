{ depot, pkgs, lib, ... }:

let
  bins = depot.nix.getBins pkgs.alacritty-stable [ "alacritty" ];

  config =
    depot.users.Profpatsch.importDhall.importDhall {
      root = ./.;
      files = [
        "alacritty.dhall"
        "solarized.dhall"
      ];
      main = "alacritty.dhall";
      deps = [ ];
    };

  config-file = lib.pipe config.alacritty-config [
    (lib.generators.toYAML { })
    (pkgs.writeText "alacritty.conf")
  ];


  alacritty = depot.nix.writeExecline "alacritty" { } (
    (lib.concatLists (lib.mapAttrsToList (k: v: [ "export" k (toString v) ]) config.alacritty-env))
    ++ [
      bins.alacritty
      "--config-file"
      config-file
      "$@"
    ]
  );

in
alacritty
