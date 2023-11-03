{ config, lib, pkgs, ... }:
{
  fonts = {
    packages = with pkgs; [
      nerdfonts
      noto-fonts-emoji
      twitter-color-emoji
    ];

    fontconfig.defaultFonts.emoji = [ "Twitter Color Emoji" ];
  };
}
