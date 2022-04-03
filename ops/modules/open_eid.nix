# NixOS module to configure the Estonian e-ID software.
{ pkgs, ... }:

{
  services.pcscd.enable = true;

  environment.systemPackages = with pkgs; [
    qdigidoc
  ];
}
