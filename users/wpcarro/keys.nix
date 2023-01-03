# wpcarro's public SSH keys
{ ... }:

rec {
  ava = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIB/5Fuo7wi8rNXVXgNaCK2X6ePCh9LQs/9h7Tj6UeXrl wpcarro@ava";
  iphone = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEU1tsRQ/cMxi9Hd7Xo+YpiWB5i6qx24EJLCEFBK4q4W wpcarro@iphone";
  kyoko = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBFILKdkNqfTP5WeoQAV6K3MdTzsDW65ToXGc6KlQ9yl wpcarro@kyoko";
  marcus = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJkNQJBXekuSzZJ8+gxT+V1+eXTm3hYsfigllr/ARXkf wpcarro@gmail.com";
  nathan = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIP2NjuP722VUgpSu5bVUPTfdVNPO8fSW0Jlas8L4up13 bill@nathan";
  tarasco = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOh+wG4f7tI0IwGyF2sLi5mPlh3JKE7KqV2ab0tlcL36 wpcarro@tarasco";

  all = [
    ava
    iphone
    kyoko
    marcus
    nathan
    tarasco
  ];
}
