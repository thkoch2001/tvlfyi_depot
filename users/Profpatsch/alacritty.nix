{ depot, pkgs, lib, ... }:

let
  bins = depot.nix.getBins pkgs.alacritty [ "alacritty" ];

  config =
    {
      alacritty-config = { font.size = 18; scolling.history = 1000000; };
      #  This disables the dpi-sensitive scaling (cause otherwise the font will be humongous on my laptop screen)
      alacritty-env.WINIT_X11_SCALE_FACTOR = 1;
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
