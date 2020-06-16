{ config, lib, pkgs, ... }:
let alsi = pkgs.callPackage ~/code/system/pkgs/alsi {};
in
{
  home.packages = [ alsi ];

  xdg.configFile."alsi/alsi.logo" = {
    source = ./nixos-logo.txt;
    force = true;
  };

  xdg.configFile."alsi/alsi.conf" = {
    force = true;
    text = ''
    #!${pkgs.perl}/bin/perl

    scalar {
      ALSI_VERSION         => "0.4.8",
      COLORS_FILE          => "/${config.home.homeDirectory}/.config/alsi/alsi.colors",
      DE_FILE              => "/${config.home.homeDirectory}/.config/alsi/alsi.de",
      DEFAULT_COLOR_BOLD   => "blue",
      DEFAULT_COLOR_NORMAL => "blue",
      DF_COMMAND           => "df -Th -x sys -x tmpfs -x devtmpfs &>/dev/stdout",
      GTK2_RC_FILE         => "/${config.home.homeDirectory}/.gtkrc-2.0",
      GTK3_RC_FILE         => "/${config.home.homeDirectory}/.config/gtk-3.0/settings.ini",
      LOGO_FILE            => "/${config.home.homeDirectory}/.config/alsi/alsi.logo",
      OUTPUT_FILE          => "/${config.home.homeDirectory}/.config/alsi/alsi.output",
      # PACKAGES_PATH        => "/var/lib/pacman/local/",
      PS_COMMAND           => "ps -A",
      USAGE_COLORS         => 0,
      USAGE_COLORS_BOLD    => 0,
      USAGE_PRECENT_GREEN  => 50,
      USAGE_PRECENT_RED    => 100,
      USAGE_PRECENT_YELLOW => 85,
      USE_LOGO_FROM_FILE   => 1,
      USE_VALUES_COLOR     => 0,
      WM_FILE              => "/${config.home.homeDirectory}/.config/alsi/alsi.wm",
    }
    '';
  };

  xdg.configFile."alsi/alsi.colors".text = ''
    #!${pkgs.perl}/bin/perl

    # Colors for alsi

    scalar {
       black   => {normal => "\e[0;30m", bold => "\e[1;30m"},
       red     => {normal => "\e[0;31m", bold => "\e[1;31m"},
       green   => {normal => "\e[0;32m", bold => "\e[1;32m"},
       yellow  => {normal => "\e[0;33m", bold => "\e[1;33m"},
       default => {normal => "\e[0;34m", bold => "\e[1;34m"},
       blue    => {normal => "\e[0;34m", bold => "\e[1;34m"},
       purple  => {normal => "\e[0;35m", bold => "\e[1;35m"},
       cyan    => {normal => "\e[0;36m", bold => "\e[1;36m"},
       white   => {normal => "\e[0;37m", bold => "\e[1;37m"},
       reset   => "\e[0m",
    }
  '';
}
