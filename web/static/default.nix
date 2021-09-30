# Expose all static assets as a folder. The derivation contains a
# `drvHash` attribute which can be used for cache-busting.
{ depot, lib, pkgs, ... }:

lib.fix(self: pkgs.runCommand "tvl-static" {} ''
  cp -r ${./.} $out
'' // {
  drvHash = lib.substring 11 32 self.drvPath;
})
