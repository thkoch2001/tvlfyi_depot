let sol = (./solarized.dhall).hex

let black = "#000000"

let white = "#ffffff"

let
    -- todo: this looks not too good
    solarized-dark =
      { --Colors (Solarized Dark)
        colors =
        { -- Default colors
          primary =
          { background = black, foreground = white }
        , -- Cursor colors
          cursor =
          { text = sol.base03, cursor = sol.base0 }
        , -- Normal colors
          normal =
          { black = sol.base02
          , red = sol.red
          , green = sol.green
          , yellow = sol.yellow
          , blue = sol.blue
          , magenta = sol.magenta
          , cyan = sol.cyan
          , white = sol.base2
          }
        , -- Bright colors
          bright =
          { black = sol.base03
          , red = sol.orange
          , green = sol.base01
          , yellow = sol.base00
          , blue = sol.base0
          , magenta = sol.violet
          , cyan = sol.base1
          , white = sol.base3
          }
        }
      }

in  { alacritty-config = { font.size = 18, scolling.history = 1000000 }
    ,   -- This disables the dpi-sensitive scaling (cause otherwise the font will be humongous on my laptop screen)
        alacritty-env
      . WINIT_X11_SCALE_FACTOR
      = 1
    }
