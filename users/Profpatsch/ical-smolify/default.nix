{
  depot,
  pkgs,
  lib,
  ...
}:

let
  cas-serve = pkgs.writers.writeHaskell "ical-smolify" {
    libraries = [
      pkgs.haskellPackages.iCalendar
      depot.users.Profpatsch.my-prelude
      depot.users.Profpatsch.execline.exec-helpers-hs
    ];
    ghcArgs = [ "-threaded" ];
  } ./IcalSmolify.hs;
in
cas-serve
