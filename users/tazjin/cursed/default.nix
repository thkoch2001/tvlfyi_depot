{ depot, ... }:

let
  inherit (depot.web) bubblegum;
in
(bubblegum.writeCGI
  {
    name = "cursed";
  } ./responder.nix) // { meta.ci.skip = true; }
