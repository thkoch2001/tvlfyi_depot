# Make Chromecasts work behind firewalls.
#
# In practice, nobody seems to know exactly how this works, so this is
# puzzled together from a bunch of random information all over the
# internet.
{ lib, ... }:

{
  services.avahi.enable = true;

  networking.firewall = {
    allowedTCPPorts = [ 8008 8009 8010 ];
    allowedUDPPorts = [ 5353 ];

    allowedUDPPortRanges = lib.singleton {
      from = 32768;
      to = 61000;
    };
  };
}
