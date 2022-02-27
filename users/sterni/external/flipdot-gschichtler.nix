{ pkgs, depot, ... }:

import depot.users.sterni.external.sources.flipdot-gschichtler { inherit pkgs; } // {
  # all targets we care about for depot
  meta.ci.targets = [
    "bahnhofshalle"
    "warteraum"
  ];
}
