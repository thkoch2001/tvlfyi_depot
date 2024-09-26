{ depot, pkgs, ... }:

depot.tools.eaglemode.buildPlugin {
  name = "avif";
  version = "canon";
  src = ./.;
  target = "PlAvif";
  extraBuildInputs = [ pkgs.libavif ];
  extraNativeBuildInputs = [ pkgs.pkg-config ];
}
