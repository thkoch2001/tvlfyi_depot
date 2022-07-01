# Laptop-specific NixOS configuration.
_:

{
  # Automatically detect location for redshift.
  services.geoclue2.enable = true;
  location.provider = "geoclue2";

  # Enable power-saving features.
  powerManagement.powertop.enable = true;

  # Backlight control command.
  programs.light.enable = true;
}

