{ depot, ... }:

(with depot.ops.nixos; [
  whitby
]) ++

(with depot.users.tazjin.nixos; [
  camden
  frog
]) ++

(with depot.users.glittershark.system.system; [
  chupacabra
])
