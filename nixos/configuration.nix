{ config, pkgs, ... }:

# TODO(wpcarro): Refactor to prefer nested attribute for configuration values
# instead of using one-liner field accessors.
{
  imports = [
    ./hardware-configuration.nix
  ];

  # TODO(wpcarro): Is this correct? I believe my laptop only supports BIOS and
  # not UEFI.
  boot.loader.grub.device = "/dev/sda";

  networking.hostName = "socrates";
  networking.wireless.enable = true;
  # Don't remove this.
  networking.useDHCP = false;
  networking.interfaces.enp2s0f1.useDHCP = true;
  networking.interfaces.wlp3s0.useDHCP = true;

  time.timeZone = "UTC";

  environment.systemPackages = with pkgs; [
    emacs
  ];

  services.openssh.enable = true;

  users.users.wpcarro = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
  };

  system.stateVersion = "20.09";
}
