{ pkgs, depot, ... }:

let
  src = depot.third_party.sources.agenix;

  agenix = import src {
    inherit pkgs;
  };
in
{
  inherit src;
  cli = agenix.agenix;

  meta.ci.targets = [ "cli" ];
}
