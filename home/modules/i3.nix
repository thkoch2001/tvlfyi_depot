{ config, lib, pkgs, ... }:
let
  mod = "Mod4";
  solarized = import ../common/solarized.nix;
  # TODO pull this out into lib
  emacsclient = eval: pkgs.writeShellScript "emacsclient-eval" ''
    msg=$(emacsclient --eval '${eval}' 2>&1)
    echo "''${msg:1:-1}"
  '';
  screenlayout = {
    home = pkgs.writeShellScript "screenlayout_home.sh" ''
      xrandr \
        --output eDP1 --mode 3840x2160 --pos 0x0 --rotate normal \
        --output DP1 --primary --mode 3840x2160 --pos 0x2160 --rotate normal \
        --output DP2 --off --output DP3 --off --output VIRTUAL1 --off
    '';
  };
in {
  options = with lib; {
    system.machine.wirelessInterface = mkOption {
      description = ''
        Name of the primary wireless interface. Used by i3status, etc.
      '';
      default = "wlp3s0";
      type = types.str;
    };

    system.machine.i3FontSize = mkOption {
      description = "Font size to use in i3 window decorations etc.";
      default = 6;
      type = types.int;
    };
  };

  config =
    let decorationFont = "MesloLGSDZ ${toString config.system.machine.i3FontSize}"; in
    {
      home.packages = with pkgs; [
        maim
        rofi
        rofi-pass
        i3status
        python38Packages.py3status
        i3lock
        dconf # for gtk
      ];

      xsession.scriptPath = ".hm-xsession";
      xsession.windowManager.i3 = {
        enable = true;
        config = {
          modifier = mod;
          keybindings = lib.mkOptionDefault rec {
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
            "${mod}+q" = "exec \"maim | xclip -selection clipboard -t image/png\"";
            "${mod}+Shift+q" = "exec \"maim -s | xclip -selection clipboard -t image/png\"";

            # Launching applications
            "${mod}+u" =
              let rofi = pkgs.writeShellScript "rofi" ''
              rofi \
                -modi 'combi' \
                -combi-modi "window,drun,ssh,run" \
                -font '${decorationFont}' \
                -show combi
            '';
              in "exec ${rofi}";

            # Passwords
            "${mod}+p" = "exec rofi-pass -font '${decorationFont}'";

            # Media
            "XF86AudioPlay" = "exec playerctl play-pause";
            "XF86AudioNext" = "exec playerctl next";
            "XF86AudioPrevious" = "exec playerctl previous";
            "XF86AudioRaiseVolume" = "exec pulseaudio-ctl up";
            "XF86AudioLowerVolume" = "exec pulseaudio-ctl down";
            "XF86AudioMute" = "exec pulseaudio-ctl mute";

            # Lock
            Pause = "exec \"sh -c 'playerctl pause; ${pkgs.i3lock}/bin/i3lock -c 222222'\"";
            F7 = Pause;

            # Screen Layout
            "${mod}+Shift+t" = "exec xrandr --auto";
            "${mod}+t" = "exec ${screenlayout.home}";
            "${mod}+Ctrl+t" = "exec ${pkgs.writeShellScript "fix_term.sh" ''
              xrandr --output eDP-1 --off && ${screenlayout.home}
            ''}";
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

              order += "external_script current_task"
              order += "external_script inbox"
              order += "spotify"
              order += "wireless ${config.system.machine.wirelessInterface}"
              # order += "ethernet enp3s0f0"
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

              spotify {
                  color_playing = "#fdf6e3"
                  color_paused = "#93a1a1"
                  format_stopped = ""
                  format_down = ""
                  format = "{title} - {artist} ({album})"
              }

              external_script inbox {
                  script_path = '${emacsclient "(grfn/num-inbox-items-message)"}'
                  format = 'Inbox: {output}'
                  cache_timeout = 120
                  color = "#93a1a1"
              }

              external_script current_task {
                  script_path = '${emacsclient "(grfn/org-current-clocked-in-task-message)"}'
                  # format = '{output}'
                  cache_timeout = 60
                  color = "#93a1a1"
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
            font = "MesloLGSDZ ${toString (config.system.machine.i3FontSize * 1.5)}";
            allow_markup = true;
            format = "<b>%s</b>\n%b";
            sort = true;
            alignment = "left";
            geometry = "600x15-40+40";
            idle_threshold = 120;
            separator_color = "frame";
            separator_height = 1;
            word_wrap = true;
            padding = 8;
            horizontal_padding = 8;
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

      gtk = {
        enable = true;
        iconTheme.name = "Adwaita";
        theme.name = "Adwaita";
      };
  };
}
