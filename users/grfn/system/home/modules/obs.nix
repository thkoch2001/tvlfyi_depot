{ config, lib, pkgs, ... }:

let
  inherit (pkgs) obs-studio;
  obs-input-overlay = pkgs.obs-studio-plugins.input-overlay;
in

{
  home.packages = [
    obs-studio
    obs-input-overlay
  ];

  xdg.configFile."obs-studio/plugins/input-overlay/bin/64bit/input-overlay.so".source =
    "${obs-input-overlay}/lib/obs-plugins/input-overlay.so";
  xdg.configFile."obs-studio/plugins/input-overlay/data".source =
    "${obs-input-overlay}/share/obs/obs-plugins/input-overlay";
}
