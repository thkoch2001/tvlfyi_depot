{ depot, config, lib, pkgs, modulesPath, ... }:

{
  imports = [
    ../modules/common.nix
    ../modules/development.nix
    "${modulesPath}/installer/scan/not-detected.nix"
    "${modulesPath}/virtualisation/amazon-image.nix"
  ];

  system.stateVersion = "22.05";

  networking.hostName = "roswell";

  boot.loader.systemd-boot.enable = lib.mkForce false;
  boot.loader.efi.canTouchEfiVariables = lib.mkForce false;

  services.openssh.settings.PasswordAuthentication = false;

  services.tailscale.enable = true;

  security.sudo.wheelNeedsPassword = false;

  environment.systemPackages = with pkgs; [
    cloud-utils
  ];
}
