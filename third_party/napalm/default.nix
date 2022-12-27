{ depot, pkgs, lib, ... }:

lib.updateManyAttrsByPath
  [
    {
      path = [ "napalm-registry" ];
      update = pkgs.haskell.lib.compose.appendPatch
        # https://github.com/nix-community/napalm/pull/52
        (pkgs.fetchpatch {
          name = "napalm-registry-hashable-1.4.patch";
          url = "https://github.com/nix-community/napalm/pull/52/commits/09ba608b4845bf1d832f562191eb6a13c12a1e2d.patch";
          sha256 = "05j73w6fhqhlvr5h7cfk342lpsi9khm3gsc5fsz3ch3iv2pcrwgk";
          stripLen = 1;
        });
    }
  ]
  (
    pkgs.callPackage depot.third_party.sources.napalm { } // {
      meta.ci.targets = [
        "napalm-registry"
      ];
    }
  )
