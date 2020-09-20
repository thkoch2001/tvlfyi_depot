{ config ? throw "not a readTree target", ... }:

let
  depotPath = "/home/multi/depot";
in
  depotPath
