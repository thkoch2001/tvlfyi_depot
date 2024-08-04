{ config, lib, pkgs, ... }:

{
  imports = [
    ./containers.nix
  ];

  security.pam.loginLimits = [
    {
      domain = "aspen";
      type = "soft";
      item = "nofile";
      value = "65535";
    }
  ];
}
