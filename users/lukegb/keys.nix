# My SSH public keys
{ ... }:

rec {
  termius = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINytpHct7PLdLNp6MoaOPP7ccBPUQKymVNMqix//Wt1f";
  porcorosso-nixos = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILid+1rq3k3k7Kbaw8X63vrPrQdanH55TucQwp3ZWfo+";
  clouvider-lon01-nix = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINQU7Y+Ha5m0ebwUjA55xXT/xbWZAWx1fVNFufle+vQj";
  lukegb-build = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICESF0H+OCxY/CfyG9VjM6iJe+VbYc4NmGjRrwPCHaD9";
  lukegb-ca = "cert-authority,principals=\"lukegb\" ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEqNOwlR7Qa8cbGpDfSCOweDPbAGQOZIcoRgh6s/J8DR";
  all = [ termius porcorosso-nixos clouvider-lon01-nix lukegb-build lukegb-ca ];
}
