# See README.md
{ depot ? import ../. {} }:

depot.third_party.nixpkgs {
  tvl = depot;
}
