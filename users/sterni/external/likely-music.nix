{ depot, pkgs, ... }:

import depot.users.sterni.external.sources.likely-music
  {
    inherit pkgs;
    inherit (depot.third_party) napalm;
  } // {
  meta.ci.targets = [
    "likely-music"
  ];
}
