{ depot, pkgs, ... }:

let
  launcher = "${pkgs.xfce4-appfinder}/bin/xfce4-appfinder --disable-server";
in
{
  mainBar = {
    layer = "top";
    position = "bottom";
    modules-left = [ "custom/start" ];

    "custom/start" = {
      format = " Start";
      on-click = "xfce4-appfinder --disable-server";
    };

    modules-right = [ "tray" "backlight" "battery" "pulseaudio" "clock" ];

    pulseaudio = {
      on-click = "pavucontrol";
      format = " "; #styling only
      states = {
        low = 1;
        medium = 40;
        high = 75;
      };
    };

    battery = {
      format = " "; # styling only
      interval = 10;
      states = {
        full = 100;
        good = 85;
        medium = 60;
        low = 40;
        warning = 20;
        critical = 10;
      };
    };

    backlight = {
      format = "{percent}%"; # styling only
      on-scroll-up = "light -A 1";
      on-scroll-down = "light -U 1";
    };

    clock.format-alt = "{:%a, %d. %b  %H:%M}";

    tray = {
      icon-size = 20;
      spacing = 10;
    };
  };
}
