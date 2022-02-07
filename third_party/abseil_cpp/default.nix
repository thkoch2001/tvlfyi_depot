{ pkgs, lib, ... }:

let inherit (pkgs) cmake fullLlvm11Stdenv;
in pkgs.abseil-cpp.override {
  stdenv = fullLlvm11Stdenv;
  cxxStandard = "17";
}
