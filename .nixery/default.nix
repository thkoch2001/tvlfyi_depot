# See README.md
{ ... }:

let depot = import (builtins.fetchGit "https://cl.tvl.fyi/depot") {};
in depot.third_party.nixpkgs.extend(_: _: {
  tvl = depot;
})
