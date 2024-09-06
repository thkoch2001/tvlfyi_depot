# Attempt at configuring reasonable font-rendering.

{ depot, pkgs, ... }:

{
  fonts = {
    packages = with pkgs; [
      corefonts
      dejavu_fonts
      font-awesome
      jetbrains-mono
      noto-fonts-cjk
      noto-fonts-color-emoji
      noto-fonts-monochrome-emoji
      depot.third_party.chicago95
    ];

    fontconfig = {
      hinting.enable = true;
      subpixel.lcdfilter = "light";

      defaultFonts = {
        monospace = [ "JetBrains Mono" ];
      };
    };
  };
}
