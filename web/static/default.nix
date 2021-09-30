# Expose all static assets as a folder. The derivation contains a
# `drvHash` attribute which can be used for cache-busting.
{ depot, lib, pkgs, ... }:

let logo = depot.web.tvl.logo;
in lib.fix(self: pkgs.runCommand "tvl-static" {} ''
  mkdir $out
  cp -r ${./.}/* $out
  cp ${logo.pastelRainbow} $out/logo-animated.svg
  cp ${logo.redPng} $out/logo-red.png
'' // {
  drvHash = lib.substring 11 32 self.drvPath;
})
