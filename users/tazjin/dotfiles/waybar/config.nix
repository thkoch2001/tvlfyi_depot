{ depot, pkgs, ... }:

let
  launcher = "${pkgs.xfce4-appfinder}/bin/xfce4-appfinder --disable-server";
in
{
  mainBar = {
    layer = "top";
    position = "bottom";
    modules-left = [ "image#start" "custom/start" ];

    "image#start" = {
      path = "${depot.third_party.chicago95}/share/icons/Chicago95/panel/24/start-here.png";
      size = 24;
      on-click = "xfce4-appfinder --disable-server";
    };

    "custom/start" = {
      format = " Start";
      on-click = "xfce4-appfinder --disable-server";
    };

    modules-right = [ "pulseaudio" "backlight" "battery" "tray" "clock" ];

    pulseaudio = {
      on-click = "pavucontrol";
      # Font "Awesome" speaker icons can't be made to render, I don't care why, emoji time.
      format = "{volume}% 🎧";
      format-muted = "{volume}% ";
    };

    battery = {
      format = "{capacity}% {icon}";
      format-icons = [ "" "" "" "" "" ];
    };

    backlight = {
      format = "{percent}% {icon}";
      format-icons = [ "" ];
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
