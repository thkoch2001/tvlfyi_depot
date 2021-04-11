{ depot, ... }:

(with depot.ops.nixos; [
  whitby
]) ++

(with depot.users.tazjin.nixos; [
  camden
  frog
  tverskoy
]) ++

(with depot.users.grfn.system.system; [
  chupacabra
  yeren
])
