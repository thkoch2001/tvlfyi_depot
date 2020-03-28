{ config, lib, pkgs, ... }:
let
  mod = "Mod4";
  solarized = import ../common/solarized.nix;
  decorationFont = "MesloLGSDZ 6";
in {
  home.packages = with pkgs; [
    maim
    rofi
    i3status
    python38Packages.py3status
  ];

  xsession.scriptPath = ".hm-xsession";
  xsession.windowManager.i3 = {
    enable = true;
    config = {
      modifier = mod;
      keybindings = lib.mkOptionDefault {
        "${mod}+h" = "focus left";
        "${mod}+j" = "focus down";
        "${mod}+k" = "focus up";
        "${mod}+l" = "focus right";
        "${mod}+semicolon" = "focus parent";

        "${mod}+Shift+h" = "move left";
        "${mod}+Shift+j" = "move down";
        "${mod}+Shift+k" = "move up";
        "${mod}+Shift+l" = "move right";

        "${mod}+Shift+x" = "kill";

        "${mod}+Return" = "exec alacritty";

        "${mod}+Shift+s" = "split h";
        "${mod}+Shift+v" = "split v";

        "${mod}+f" = "fullscreen";

        "${mod}+Shift+r" = "exec home-manager switch";

        "${mod}+r" = "mode resize";

        # Marks
        "${mod}+Shift+m" = ''exec i3-input -F "mark %s" -l 1 -P 'Mark: ' '';
        "${mod}+m" = ''exec i3-input -F '[con_mark="%s"] focus' -l 1 -P 'Go to: ' '';

        # Screenshots
        "${mod}+q" = "exec maim";
        "${mod}+Shift+q" = "exec \"maim -s | xclip -selection clipboard -t image/png\"";

        # Launching applications
        "${mod}+u" =
          let rofi = pkgs.writeShellScript "rofi" ''
            rofi \
              -modi 'combi' \
              -combi-modi "window,drun,ssh,run" \
              -font 'MesloLGSDZ 10' \
              -show combi
          '';
          in "exec ${rofi}";

        # Media
        "XF86AudioPlay" = "exec playerctl play-pause";
        "XF86AudioNext" = "exec playerctl next";
        "XF86AudioPrevious" = "exec playerctl previous";
        "XF86AudioRaiseVolume" = "exec pulseaudio-ctl up";
        "XF86AudioLowerVolume" = "exec pulseaudio-ctl down";
        "XF86AudioMute" = "exec pulseaudio-ctl mute";

        # Screen Layout
        "${mod}+Shift+t" = "exec xrandr --auto";
        # TODO
        # $mod+t exec /home/griffin/.screenlayout/work.sh
        # $mod+Ctrl+t exec /home/griffin/bin/fix_screen.sh
      };

      fonts = [ decorationFont ];

      colors = with solarized; rec {
        focused = {
          border = base01;
          background = base01;
          text = base3;
          indicator = red;
          childBorder = base02;
        };
        focusedInactive = focused // {
          border = base03;
          background = base03;
          # text = base1;
        };
        unfocused = focusedInactive;
        background = base03;
      };

      modes.resize = {
        l = "resize shrink width 5 px or 5 ppt";
        k = "resize grow height 5 px or 5 ppt";
        j = "resize shrink height 5 px or 5 ppt";
        h = "resize grow width 5 px or 5 ppt";

        Return = "mode \"default\"";
      };

      bars = [{
        statusCommand =
          let i3status-conf = pkgs.writeText "i3status.conf" ''
            general {
                output_format = i3bar
                colors = true
                color_good = "#859900"

                interval = 1
            }

            order += "mpd"
            order += "wireless wlp3s0"
            order += "ethernet enp3s0f0"
            order += "cpu_usage"
            order += "battery 0"
            # order += "volume master"
            order += "time"

            mpd {
                format = "%artist - %album - %title"
            }

            wireless wlp3s0 {
                format_up = "W: (%quality - %essid - %bitrate) %ip"
                format_down = "W: -"
            }

            ethernet enp3s0f0 {
                format_up = "E: %ip"
                format_down = "E: -"
            }

            battery 0 {
                format = "%status %percentage"
                path = "/sys/class/power_supply/BAT%d/uevent"
                low_threshold = 10
            }

            cpu_usage {
                format = "CPU: %usage"
            }

            load {
                format = "%5min"
            }

            time {
                format = "    %a %h %d ⌚   %I:%M     "
            }

            # volume master {
            #     format = "☊ %volume"
            #     format_muted = "☊ X"
            #     device = "default"
            #     mixer_idx = 0
            # }
          '';
          in "py3status -c ${i3status-conf}";
        fonts = [ decorationFont ];
        position = "top";
        colors = with solarized; rec {
          background = base03;
          statusline = base3;
          separator = base1;
          activeWorkspace = {
            border = base03;
            background = base1;
            text = base3;
          };
          focusedWorkspace = activeWorkspace;
          inactiveWorkspace = activeWorkspace // {
            background = base01;
          };
          urgentWorkspace = activeWorkspace // {
            background = red;
          };
        };
      }];
    };
  };

  services.dunst = {
    enable = true;
    settings = with solarized; {
      global = {
        font = "Meslo 6";
        allow_markup = true;
        format = "<b>%s</b>\n%b";
        sort = true;
        alignment = "left";
        geometry = "600x5-30+20";
        idle_threshold = 120;
        separator_color = "frame";
      };

      frame = {
        width = 0;
        color = "#aaaaaa";
      };

      shortcuts = {
        close = "ctrl+space";
        close_all = "ctrl+shift+space";
        history = "ctrl+grave";
        context = "ctrl+shift+period";
      };

      urgency_low = {
        background = base03;
        foreground = base3;
        timeout = 5;
      };

      urgency_normal = {
        background = base02;
        foreground = base3;
        timeout = 7;
      };

      urgency_critical = {
        background = red;
        foreground = base3;
        timeout = 0;
      };
    };
  };
}
