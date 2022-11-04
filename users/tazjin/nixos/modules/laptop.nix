# Configuration specifically for laptops that move around.
{ ... }:

{
  time.timeZone = "Europe/Stockholm";

  # Automatically detect location for redshift & so on ...
  services.geoclue2.enable = true;
  location.provider = "geoclue2";

  # Enable power-saving features.
  services.tlp.enable = true;

  programs.light.enable = true;
}
