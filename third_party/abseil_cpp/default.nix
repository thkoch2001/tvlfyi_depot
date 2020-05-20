{ pkgs, ... }:

pkgs.originals.abseil-cpp.overrideAttrs(_: {
  version = "20200519-768eb2ca";
  src = ./.;
})
