{ config, lib, pkgs, ... }:
{
  home.packages = with pkgs; [
    alacritty 
  ];

  programs.alacritty = {
    enable = true;
    settings = {
      font.size = 6;
      font.normal.family = "Meslo LGSDZ Nerd Font";

      draw_bold_text_with_bright_colors = false;

      colors = with import ../common/solarized.nix; rec {
        # Default colors
        primary = {
          background = base3;
          foreground = base00;
        };

        cursor = {
          text = base3;
          cursor = base00;
        };

        # Normal colors
        normal = {
          inherit red green yellow blue magenta cyan;
          black = base02;
          white = base2;
        };

        # Bright colors
        # bright = normal;
        bright = {
          black = base03;
          red = orange;
          green = base01;
          yellow = base00;
          blue = base0;
          magenta = violet;
          cyan = base1;
          white = base3;
        };
      };
    };
  };
}
