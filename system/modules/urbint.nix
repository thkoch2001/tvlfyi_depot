{ config, pkgs, ... }:
{
  virtualisation.docker.enable = true;

  nix = rec {
    binaryCaches = [ "https://nix.urbinternal.com" ];
    trustedBinaryCaches = binaryCaches;
    trustedUsers = [ "griffin" ];
    requireSignedBinaryCaches = false;
  };

  services.openvpn.servers.urbint = {
    config = ''
      config /root/openvpn/urbint.conf
    '';
    autoStart = false;
  };
}
