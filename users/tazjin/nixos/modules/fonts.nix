# Attempt at configuring reasonable font-rendering.

{ pkgs, ... }:

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
