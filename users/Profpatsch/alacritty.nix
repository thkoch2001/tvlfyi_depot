{ depot, pkgs, lib, ... }:

let
  bins = depot.nix.getBins pkgs.alacritty [ "alacritty" ];

  config =
    {
      alacritty-config = { font.size = 18; scrolling.history = 100000; };
      #  This disables the dpi-sensitive scaling (cause otherwise the font will be humongous on my laptop screen)
      alacritty-env.WINIT_X11_SCALE_FACTOR = 1;
    };


  config-file = (pkgs.formats.toml { }).generate "alacritty.conf" config.alacritty-config;

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
