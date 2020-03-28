{ config, lib, pkgs, ... }:
let alsi = pkgs.callPackage ../../pkgs/alsi {};
in
{
  home.packages = [ alsi ];

  home.file.".config/alsi/alsi.logo" = {
    source = ./nixos-logo.txt;
    force = true;
  };

  home.file.".config/alsi/alsi.conf" = {
    text = ''
    #!${pkgs.perl}/bin/perl

    scalar {
      ALSI_VERSION         => "0.4.8",
      COLORS_FILE          => "/home/grfn/.config/alsi/alsi.colors",
      DE_FILE              => "/home/grfn/.config/alsi/alsi.de",
      DEFAULT_COLOR_BOLD   => "blue",
      DEFAULT_COLOR_NORMAL => "blue",
      DF_COMMAND           => "df -Th -x sys -x tmpfs -x devtmpfs &>/dev/stdout",
      GTK2_RC_FILE         => "/home/grfn/.gtkrc-2.0",
      GTK3_RC_FILE         => "/home/grfn/.config/gtk-3.0/settings.ini",
      LOGO_FILE            => "/home/grfn/.config/alsi/alsi.logo",
      OUTPUT_FILE          => "/home/grfn/.config/alsi/alsi.output",
      # PACKAGES_PATH        => "/var/lib/pacman/local/",
      PS_COMMAND           => "ps -A",
      USAGE_COLORS         => 0,
      USAGE_COLORS_BOLD    => 0,
      USAGE_PRECENT_GREEN  => 50,
      USAGE_PRECENT_RED    => 100,
      USAGE_PRECENT_YELLOW => 85,
      USE_LOGO_FROM_FILE   => 1,
      USE_VALUES_COLOR     => 0,
      WM_FILE              => "/home/grfn/.config/alsi/alsi.wm",
    }
    '';
    force = true;
  };
}
