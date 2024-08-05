{ depot, pkgs, lib, ... }:

let
  ical-smolify = pkgs.writers.writeHaskell "ical-smolify"
    {
      libraries = [
        pkgs.haskellPackages.iCalendar
        depot.users.Profpatsch.my-prelude
        depot.users.Profpatsch.execline.exec-helpers-hs

      ];
      ghcArgs = [ "-threaded" ];
    } ./IcalSmolify.hs;

in

ical-smolify.overrideAttrs (old: {
  meta = lib.recursiveUpdate old.meta or { } {
    # Dependency iCalendar no longer builds in nixpkgs due to a lack of maintenance upstream
    # https://github.com/nixos/nixpkgs/commit/13d10cc6e302e7d5800c6a08c1728b14c3801e26
    ci.skip = true;
  };
})
