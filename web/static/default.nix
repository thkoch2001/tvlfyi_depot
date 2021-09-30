# Expose all static assets as a folder. The derivation contains a
# `drvHash` attribute which can be used for cache-busting.
{ depot, lib, pkgs, ... }:

let storeDirLength = with builtins; (stringLength storeDir) + 1;
in lib.fix(self: pkgs.runCommand "tvl-static" {
  passthru = {
    drvHash = lib.substring storeDirLength 32 self.drvPath;
  };
} ''
  cp -r ${./.} $out
'')
