# wpcarro's public SSH keys
{ ... }:

rec {
  diogenes = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILFDRfpNXDxQuTJAqVg8+Mm/hOfE5VAJP+Lpw9kA5cDG wpcarro@gmail.com";
  marcus = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJkNQJBXekuSzZJ8+gxT+V1+eXTm3hYsfigllr/ARXkf wpcarro@gmail.com";
  nathan = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIP2NjuP722VUgpSu5bVUPTfdVNPO8fSW0Jlas8L4up13 bill@nathan";
  ava = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIB/5Fuo7wi8rNXVXgNaCK2X6ePCh9LQs/9h7Tj6UeXrl wpcarro@ava";

  all = [ diogenes marcus nathan ava ];
}
