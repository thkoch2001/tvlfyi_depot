let sol = (./solarized.dhall).hex

let solarized-dark =
      { --Colors (Solarized Dark)
        colors =
        { -- Default colors
          primary =
          { background = sol.base03, foreground = sol.base0 }
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

in  { font.size = 12 } // solarized-dark
