{ config, lib, pkgs, ... }:

{

  environment.systemPackages = with pkgs; [
    rtl-sdr
  ];

  services.udev.packages = with pkgs; [
    rtl-sdr
  ];

  # blacklist for rtl-sdr
  boot.blacklistedKernelModules = [
    "dvb_usb_rtl28xxu"
  ];
}
