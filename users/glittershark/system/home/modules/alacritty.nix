{ config, lib, pkgs, ... }:

let

  # TODO(grfn): Find a way to extract this to third_party without too much
  # effort
  alacrittyNixpkgs = import (pkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "cd9286194a5597e28b8bfb747418dd6af1567eb3";
    sha256 = "0rwhq7cy4xy25gg2in6vah55psdizz7r187zy7z4c2a7pj2hs7ak";
  }) {};

in

{
  programs.alacritty = {
    enable = true;
    package = alacrittyNixpkgs.alacritty;
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

        vi_mode_cursor.cursor = red;
      };
    };
  };
}
