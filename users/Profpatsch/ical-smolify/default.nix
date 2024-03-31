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

# iCalendar is unmaintained and maralorn has given up duct taping it together:
  # https://github.com/nixos/nixpkgs/commit/13d10cc6e302e7d5800c6a08c1728b14c3801e26
depot.nix.readTree.skipTarget ical-smolify
