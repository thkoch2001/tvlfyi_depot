# Configuration for machines with HiDPI displays, which are a total
# mess, of course.
{ ... }:

{
  # Expose a variable to all programs that might be interested in the
  # screen settings to do conditional initialisation (mostly for Emacs).
  environment.variables.HIDPI_SCREEN = "true";

  # Ensure a larger font size in early boot stage.
  hardware.video.hidpi.enable = true;

  # Bump DPI across the board.
  # TODO(tazjin): This should actually be set per monitor, but I
  # haven't yet figured out the right interface for doing that.
  services.xserver.dpi = 192;
}
