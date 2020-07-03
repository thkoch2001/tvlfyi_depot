{ depot, ... }:

(with depot.users.tazjin.nixos; [
  camden
  frog
]) ++

(with depot.users.glittershark.system.system; [
  chupacabra
])
