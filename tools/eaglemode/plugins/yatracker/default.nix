{ depot, pkgs, ... }:

let
  em = depot.tools.eaglemode;
  emSrc = with pkgs; srcOnly eaglemode;
in
(em.buildPlugin {
  name = "yatracker";
  version = "canon";
  src = ./.;
  target = "PlYaTracker";
}).overrideAttrs (_: {
  postInstall = ''
    mkdir -p $out/icons
    ${pkgs.imagemagick}/bin/convert $src/logo.webp $out/icons/yandex-tracker.tga
  '';
})

