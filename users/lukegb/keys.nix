# My SSH public keys
{ ...
}:
rec
  {
  termius = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINytpHct7PLdLNp6MoaOPP7ccBPUQKymVNMqix//Wt1f";
  porcorosso-wsl = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMhQ3yjf59eQjOfVXzXz5u8BS5c6hdL1yY8GqccaIjx3";
  porcorosso-nixos = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILid+1rq3k3k7Kbaw8X63vrPrQdanH55TucQwp3ZWfo+";
  clouvider-lon01-nix = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINQU7Y+Ha5m0ebwUjA55xXT/xbWZAWx1fVNFufle+vQj";
  all = [ termius porcorosso-wsl porcorosso-nixos clouvider-lon01-nix ];
}
