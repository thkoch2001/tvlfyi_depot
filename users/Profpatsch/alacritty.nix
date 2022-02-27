{ depot, pkgs, lib, ... }:

let
  bins = depot.nix.getBins pkgs.alacritty [ "alacritty" ];

  config = lib.pipe { } [
    (_: depot.users.Profpatsch.importDhall.importDhall {
      root = ./.;
      files = [
        "alacritty.dhall"
        "solarized.dhall"
      ];
      main = "alacritty.dhall";
      deps = [ ];
    })
    (lib.generators.toYAML { })
    (pkgs.writeText "alacritty.conf")
  ];


  alacritty = depot.nix.writeExecline "alacritty" { } [
    bins.alacritty
    "--config-file"
    config
    "$@"
  ];

in
alacritty
