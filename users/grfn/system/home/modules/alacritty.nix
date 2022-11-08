{ config, lib, pkgs, ... }:

let
  inherit (builtins)
    mapAttrs
    substring
    stringLength
    ;
in {
  programs.alacritty = {
    enable = true;
    settings = {
      font.size = 6;
      font.normal.family = "Meslo LGSDZ Nerd Font";

      draw_bold_text_with_bright_colors = false;

      key_bindings = [
        {
          key = "Escape";
          mods = "Control";
          action = "ToggleViMode";
        }
      ];

      colors = with (
        let selenized = import ../common/selenized.nix;
        in mapAttrs
          (_: color: "0x${substring 1 (stringLength color) color}")
          selenized
      ); {
        # Default colors
        primary = {
          background = bg;
          foreground = fg;
        };

        # cursor = {
        #   text = bg;
        #   cursor = fg;
        # };

        # Normal colors
        normal = {
          inherit black red green yellow blue magenta cyan white;
        };

        # Bright colors
        # bright = normal;
        bright = {
          black = br_black;
          red = br_red;
          green = br_green;
          yellow = br_yellow;
          blue = br_blue;
          magenta = br_magenta;
          cyan = br_cyan;
          white = br_white;
        };

        vi_mode_cursor.cursor = red;
      };
    };
  };
}
