# Attempt at configuring reasonable font-rendering.

{ pkgs, ... }:

{
  fonts = {
    packages = with pkgs; [
      corefonts
      dejavu_fonts
      jetbrains-mono
      noto-fonts-cjk
      noto-fonts-emoji
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
