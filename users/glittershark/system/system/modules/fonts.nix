{ config, lib, pkgs, ... }:
{
  fonts = {
    fonts = with pkgs; [
      nerdfonts
      noto-fonts-emoji
      twitter-color-emoji
    ];

    fontconfig.defaultFonts.emoji = ["Twitter Color Emoji"];
  };
}
