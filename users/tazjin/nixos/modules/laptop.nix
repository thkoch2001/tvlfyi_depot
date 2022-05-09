# Configuration specifically for laptops that move around.
{ ... }:

{
  # Automatically detect location to use for redshift
  location.provider = "geoclue2";

  # Enable power-saving features.
  services.tlp.enable = true;

  programs.light.enable = true;
}
