{ depot, ... }:

{
  chupacabra = (depot.third_party.nixos {
    configuration = import ./machines/chupacabra.nix;
  }).system;
}
