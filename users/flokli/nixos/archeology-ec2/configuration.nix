{ depot, pkgs, modulesPath, ... }:

{
  imports = [
    "${modulesPath}/virtualisation/amazon-image.nix"
    ../profiles/archeology.nix
  ];

  networking.hostName = "archeology-ec2";

  system.stateVersion = "23.05"; # Did you read the comment?
}

