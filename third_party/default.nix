{ pkgs, unstable, briefcase, depot, ... }:

# Exposing these to be available as briefcase.third_party.pkgs for example.

{ inherit pkgs unstable briefcase depot; }
