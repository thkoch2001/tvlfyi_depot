# Configuration specifically for laptops that move around.
{ ... }:

{
  # Automatically detect location for redshift & timezone settings.
  services.geoclue2.enable = true;
  location.provider = "geoclue2";
  services.localtime.enable = true;

  # Enable power-saving features.
  services.tlp.enable = true;

  programs.light.enable = true;
}
