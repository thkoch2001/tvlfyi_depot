# My SSH public keys
{ ... }:

rec {
  frog = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKMZzRdcrHTuCPoaFy36MPr5IW/hnImlse/OBOn6udL/ tazjin@frog";
  vauxhall = "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBHs+9QfZTD5qGsBQaWqp5whmXJ9qy/m9swE2M9QBaIQVoIYGemq3HXTzrQ6XekwudJCltP4EpM7h/Qc+Or309Yw=";
  s10e = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDf7CNlYoauHcSYsMNnCZt5h9QSYH/7keYkg8g3hT32+";

  all = [ frog vauxhall s10e ];
}
