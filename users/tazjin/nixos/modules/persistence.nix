# Configuration for persistent (non-home) data.
{ depot, pkgs, lib, ... }:

{
  imports = [
    (depot.third_party.sources.impermanence + "/nixos.nix")
  ];

  environment.persistence."/persist" = {
    directories = [
      "/etc/NetworkManager/system-connections"
      "/etc/mullvad-vpn"
      "/var/cache/mullvad-vpn"
      "/var/lib/bluetooth"
      "/var/lib/systemd/coredump"
      "/var/lib/tailscale"
      "/var/log"
    ];

    files = [
      "/etc/machine-id"
    ];
  };

  programs.fuse.userAllowOther = true;
}
