{ alacritty-config = { font.size = 18, scolling.history = 1000000 }
    ,   -- This disables the dpi-sensitive scaling (cause otherwise the font will be humongous on my laptop screen)
        alacritty-env
      . WINIT_X11_SCALE_FACTOR
      = 1
    }
